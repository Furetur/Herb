open Base
module V = Ollvm.Ez.Value
module I = Ollvm.Ez.Instr
module B = Ollvm.Ez.Block
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer
open Ir

type locals_map = (ident, V.t, String.comparator_witness) Map.t
type labels_map = (Label.label, V.t, Label.Comparator.comparator_witness) Map.t

type state = {
  locals_map : locals_map;
  labels_map : labels_map;
  instructions : Ollvm_ast.instr list;
  ll_module : M.t;
  next_reg_id : int;
}

module ThisPass = Passes.NoErrors (struct
  type t = state
end)

open ThisPass

(* ----- Helpers ----- *)

let ( <-- ) = I.( <-- )
let int_t = T.i32
let int_ptr_t = T.pointer T.i32

(* ----- State ----- *)

let get_ll_ptr ident =
  let* s = get in
  return (Map.find_exn s.locals_map ident)

let get_ll_label label =
  let* s = get in
  return (Map.find_exn s.labels_map label)

let add_instr instr =
  let* s = get in
  set { s with instructions = instr :: s.instructions }

let make_temp_reg =
  let* s = get in
  let name = Printf.sprintf "temp_%d" s.next_reg_id in
  let m, r = M.local s.ll_module int_t name in
  set { s with next_reg_id = s.next_reg_id + 1; ll_module = m } *> return r

let as_temp_reg instr =
  let* reg = make_temp_reg in
  add_instr (reg <-- instr) *> return reg

(* ----- Pass ----- *)

let rec pass_expr = function
  | Constant (ConstantInt i) -> return (V.i32 i)
  | Ident ident -> 
    let* ptr = get_ll_ptr ident in
    as_temp_reg (I.load ptr ~align:(Some 8))
  | Binop (l, op, r) ->
      let* l = pass_expr l in
      let* r = pass_expr r in
      let instr =
        match op with
        | BinopIntPlus -> I.add l r
        | BinopIntMinus -> I.sub l r
        | BinopIntMul -> I.mul l r
        | BinopIntDiv -> I.sdiv l r
        | BinopIntMod -> I.srem l r
      in
      as_temp_reg instr
  | Builtin _ -> failwith "builtins not supported"

let pass_stmt = function
  | Assign (LvalueIdent ident, expr) ->
      let* ptr = get_ll_ptr ident in
      let* v = pass_expr expr in
      let _, instr = I.store v ptr in
      add_instr instr
  | ExprStmt e -> pass_expr e *> return ()


let pass_terminator = function
  | Jump label ->
      let* ll_label = get_ll_label label in
      add_instr (I.br1 ll_label) *> return ()
  | Return expr ->
      let* v = pass_expr expr in
      add_instr (I.ret v) *> return ()
  | CondBranch { cond = expr; if_true; if_false } ->
      let* ll_true = get_ll_label if_true in
      let* ll_false = get_ll_label if_false in
      let* v = pass_expr expr in
      add_instr (I.br v ll_true ll_false) *> return ()


let gen_block m next_reg_id locals_map labels_map { label; body; terminator } =
  let ll_label = Map.find_exn labels_map label in
  let pass_block body terminator =
    many body ~f:pass_stmt *> pass_terminator terminator *>
    let* s = get in
    return (B.block ll_label (List.rev s.instructions))
  in
  let block =
    run_pass
      (pass_block body terminator)
      ~init:{ locals_map; labels_map; instructions = []; ll_module = m; next_reg_id }
  in
  (m, next_reg_id, block)


let gen_callframe_init_block m locals_map labels_map next_label =
  let m, ll_this_label = M.local m T.label "call_frame_init" in
  let ll_next_label = Map.find_exn labels_map next_label in
  let alloca_instructions =
    Map.data locals_map |> List.map ~f:(fun reg -> reg <-- I.alloca int_t)
  in
  let instructions = alloca_instructions @ [I.br1 ll_next_label] in
  (m, B.block ll_this_label instructions)

let gen_entry m { locals; entry_block; blocks } =
  let build_locals_map m locals : M.t * locals_map =
    let create_reg (m, locals_map) ident =
      let m, reg = M.local m int_ptr_t ident in
      let locals_map = Map.set locals_map ~key:ident ~data:reg in
      (m, locals_map)
    in
    let m, locals_map =
      List.fold_left locals ~init:(m, Map.empty (module String)) ~f:create_reg
    in
    (m, locals_map)
  in
  let build_labels_map m labels : M.t * labels_map =
    let create_ll_label (m, labels_map) label =
      let m, ll_label = M.local m T.label (Label.show_label label) in
      let labels_map = Map.set labels_map ~key:label ~data:ll_label in
      (m, labels_map)
    in
    List.fold labels
      ~init:(m, Map.empty (module Label.Comparator))
      ~f:create_ll_label
  in
  let m, locals_map = build_locals_map m locals in
  let all_labels = List.map (entry_block :: blocks) ~f:(fun b -> b.label) in
  let m, labels_map = build_labels_map m all_labels in
  (* Generate *)
  let m, callframe_init_block = gen_callframe_init_block m locals_map labels_map entry_block.label in
  let folder (m, next_reg_id, blocks) block =
    let (m, next_reg_id, block) = gen_block m next_reg_id locals_map labels_map block in
    (m, next_reg_id, block::blocks)
  in

  let m, _, ll_blocks = List.fold (entry_block::blocks) ~init:(m, 0, []) ~f:folder in
  let ll_blocks = List.rev ll_blocks in
  let m, main_func = M.global m int_t "main" in
  let main_func_impl =
    B.define main_func [] (callframe_init_block :: ll_blocks)
  in
  let m = M.definition m main_func_impl in
  m

let gen_module ({ entry } : ir) : M.t =
  let m = M.init "name" ("arm64", "apple", "macosx13.0.0") "" in
  gen_entry m entry
