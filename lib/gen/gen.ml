module V = Ollvm.Ez.Value
module I = Ollvm.Ez.Instr
module B = Ollvm.Ez.Block
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer
open Loc
open Typ
open Typed_ast

let ( <-- ) = I.( <-- )

type state = {
  modul : M.t;
  cur_basic_block : B.block;
  generated_code : B.block list;
}

module Pass = Pass.Pass (struct
  type t = state
end)

open Pass

(* ----- Helpers ------ *)

let not_implemented () = failwith "Not implemented"

(* ----- State ------ *)

let make_lab name =
  let* s = get in
  let m, lab = M.local s.modul T.label name in
  let* _ = set { s with modul = m } in
  return lab

let make_reg typ ?(name = "") =
  let* s = get in
  let m, reg = M.local s.modul typ name in
  let* _ = set { s with modul = m } in
  return reg

let gen_instr instr =
  let* s = get in
  let bb_lab, bb_code = s.cur_basic_block in
  set { s with cur_basic_block = (bb_lab, Util.pushback bb_code instr) }

let in_basicblock lab f =
  let* old_s = get in
  let old_bb = old_s.cur_basic_block in
  let _, lab_ident = V.ident lab in
  let* _ = set { old_s with cur_basic_block = (lab_ident, []) } in
  let* ret = f in
  let* new_s = get in
  let new_bb = new_s.cur_basic_block in
  let* _ =
    set
      {
        new_s with
        generated_code = new_bb :: new_s.generated_code;
        cur_basic_block = old_bb;
      }
  in
  return ret

(* ----- Pass ----- *)

let gen_expr { value = { node = expr; _ }; _ } =
  match expr with
  | TLiteral (TInt i) -> return (V.i32 i)
  | _ -> not_implemented ()

let rec gen_block { value = block; _ } =
  match block with
  | [ { value = TExprStmt expr; _ } ] -> gen_expr expr
  | _ -> not_implemented ()

and gen_func m name block =
  let gen_func_body =
    let* v = gen_block block in
    let* _ = gen_instr (I.ret v) in
    let* s = get in
    return (s.modul, Util.pushback s.generated_code s.cur_basic_block)
  in

  let m, func = M.global m T.i32 name in
  let m, entry_bb = M.local m T.label "entry" in
  let init_state =
    { modul = m; cur_basic_block = B.block entry_bb []; generated_code = [] }
  in
  let res = run_pass gen_func_body ~init:init_state in
  match res with
  | Error _ -> assert false
  | Ok (m, code) ->
      let func_def = B.define func [] code in
      let m = M.definition m func_def in
      m

let gen_entry m entry = gen_func m "main" entry

let gen_module ({ loc = _; value = { entry; _ } } : Typed_ast.typed_ast) =
  let m = M.init "name" ("arm64", "apple", "macosx13.0.0") "" in
  let m = gen_entry m entry in
  m
