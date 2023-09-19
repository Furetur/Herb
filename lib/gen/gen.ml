open Base
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

module Pass = Pass.NoErrors (struct
  type t = state
end)

open Pass

(* ----- Helpers ------ *)

let ignore_loc f { value; _ } = f value
let not_implemented () = failwith "Not implemented"
let unreachable () = failwith "Unreachable"
let internal_err = Printf.failwithf

let assert_type expected_typ { typ = actual_typ; _ } =
  if not (Typ.equal_typ expected_typ actual_typ) then
    internal_err "Expected type '%s', but received '%s'"
      (Typ.show_typ expected_typ)
      (Typ.show_typ actual_typ) ()

let assert_type' expected_typ { value = typed; _ } =
  assert_type expected_typ typed

let assert_types_eq typ1 typ2 =
  if not (Typ.equal_typ typ1 typ2) then
    internal_err "Expected types '%s' and '%s' to be equal" (Typ.show_typ typ1)
      (Typ.show_typ typ2) ()

(* ----- State ------ *)

let make_lab name =
  let* s = get in
  let m, lab = M.local s.modul T.label name in
  let* _ = set { s with modul = m } in
  return lab

let make_reg ?(name = "") typ =
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

let gen_literal = function
  | TInt i -> return (V.i32 i)
  | TBool true -> return (V.i1 1)
  | TBool false -> return (V.i1 0)
  | _ -> not_implemented ()

let rec gen_unop_expr unop expr : V.t t =
  assert_type' Typ.Bool expr;
  let* v = gen_expr expr in
  match unop with
  | Ast_operators.ANot ->
      let* reg = make_reg T.i1 in
      let* _ = gen_instr (reg <-- I.xor v (V.i1 1)) in
      return reg

and gen_binop_expr lexpr binop rexpr : V.t t =
  let gen_binop instr =
    let* l = gen_expr lexpr in
    let* r = gen_expr rexpr in
    let* reg = make_reg T.i32 in
    let* _ = gen_instr (reg <-- instr l r) in
    return reg
  in
  let gen_binop_with_typecheck ~ltyp ~rtyp instr =
    assert_type' ltyp lexpr;
    assert_type' rtyp rexpr;
    gen_binop instr
  in
  let equality instr =
    let { value = { typ = ltyp; _ }; _ } = lexpr in
    let { value = { typ = rtyp; _ }; _ } = rexpr in
    assert_types_eq ltyp rtyp;
    gen_binop instr
  in

  let arithmetic instr =
    gen_binop_with_typecheck ~ltyp:Typ.Int ~rtyp:Typ.Int instr
  in
  let cmp instr = gen_binop_with_typecheck ~ltyp:Typ.Int ~rtyp:Typ.Int instr in
  let logic instr =
    gen_binop_with_typecheck ~ltyp:Typ.Bool ~rtyp:Typ.Bool instr
  in

  let open Ast_operators in
  match binop with
  | APlus -> arithmetic I.add
  | AMinus -> arithmetic I.sub
  | AMul -> arithmetic I.mul
  | ADiv -> arithmetic I.sdiv
  | AMod -> arithmetic I.srem
  | ALt -> cmp I.slt
  | ALte -> cmp I.sle
  | AGt -> cmp I.sgt
  | AGte -> cmp I.sge
  | AEq -> equality I.eq
  | ANeq -> equality I.ne
  | AOr -> logic I.or_
  | AAnd -> logic I.and_

and gen_expr { value = { node = expr; _ }; _ } : V.t t =
  match expr with
  | TLiteral lit -> gen_literal lit
  | TUnOp (unop, expr) -> gen_unop_expr unop expr
  | TBinOp (l, binop, r) -> gen_binop_expr l binop r
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
  let m, code = run_pass gen_func_body ~init:init_state in
  let func_def = B.define func [] code in
  let m = M.definition m func_def in
  m

let gen_entry m entry = gen_func m "main" entry

let gen_module ({ loc = _; value = { entry; _ } } : Typed_ast.typed_ast) =
  let m = M.init "name" ("arm64", "apple", "macosx13.0.0") "" in
  let m = gen_entry m entry in
  m
