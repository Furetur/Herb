open Base
module M = Ollvm.Ez.Module
module V = Ollvm.Ez.Value
module B = Ollvm.Ez.Block
module I = Ollvm.Ez.Instr
open Ir
open Ll_defs
open Ll_symtab

(* ----- Prepare LL Function *)

let prepare_locals m { Ir.locals = ir_locals; _ } : M.t * locals_symtab =
  let create_reg (m, locals_map) ident =
    let m, reg = M.local m int_ptr_t (Ident.mangle ident) in
    let locals_map = Map.set locals_map ~key:ident ~data:reg in
    (m, locals_map)
  in
  let m, locals_map =
    List.fold_left ir_locals
      ~init:(m, Map.empty (module Ident.Comparator))
      ~f:create_reg
  in
  (m, locals_map)

let prepare_labels m { entry_block; blocks; _ } : M.t * labels_symtab =
  let ir_labels = entry_block.label :: List.map blocks ~f:(fun x -> x.label) in
  let create_ll_label (m, labels_map) label =
    let m, ll_label = M.local m T.label (Label.show_label label) in
    let labels_map = Map.set labels_map ~key:label ~data:ll_label in
    (m, labels_map)
  in
  List.fold ir_labels
    ~init:(m, Map.empty (module Label.Comparator))
    ~f:create_ll_label

let gen_callframe_init_block m fsymtab entry_label =
  let m, ll_this_label = M.local m T.label "callframe_init" in
  (* TODO: put 'entry_label' in fsymtab to avoid this unsafe operation *)
  let ll_entry_label = lookup_label fsymtab entry_label in
  let alloca_instructions =
    Map.data fsymtab.locals |> List.map ~f:(fun reg -> reg <-- I.alloca int_t)
  in
  let instructions = alloca_instructions @ [ I.br1 ll_entry_label ] in
  (m, B.block ll_this_label instructions)

let prepare_func m func_body =
  let m, locals = prepare_locals m func_body in
  let m, labels = prepare_labels m func_body in
  let fsymtab = { locals; labels } in
  let m, init_block =
    gen_callframe_init_block m fsymtab func_body.entry_block.label
  in
  (m, fsymtab, init_block)

let gen_entry m msymtab func_body =
  (* Generate code *)
  let m, fsymtab, init_block = prepare_func m func_body in
  let blocks = func_body.entry_block :: func_body.blocks in
  let m, ll_rest_blocks = Ll_instr.gen_blocks msymtab fsymtab m blocks in
  let ll_blocks = init_block :: List.rev ll_rest_blocks in
  (* Define the function *)
  let m, func_name = M.global m int_t "main" in
  let m = M.definition m (B.define func_name [] ll_blocks) in
  m
