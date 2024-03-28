open Base
module M = Ollvm.Ez.Module
module B = Ollvm.Ez.Block
module I = Ollvm.Ez.Instr
module T = Ollvm.Ez.Type
open Ir
open Ll_defs
open Ll_symtab

type state = {
  (* Read-only symtabs *)
  msymtab : module_symtab;
  fsymtab : func_symtab;
  (* Mutable state *)
  m : M.t;
  instrs : Ollvm.Ast.instr list;
}

module Dsl = Pass_dsl.No_errors (struct
  type t = state
end)

open Dsl

(* ----- Helpers ----- *)

let get_builtins_symtab =
  let* s = get in
  return s.msymtab.builtins

let add_instr instr =
  let* s = get in
  set { s with instrs = instr :: s.instrs }

let resolve_ident ident =
  let* s = get in
  return (lookup_local s.fsymtab ident)

let resolve_label label =
  let* s = get in
  return (lookup_label s.fsymtab label)

let make_temp_reg ll_type =
  let* s = get in
  let m, r = M.local s.m ll_type "" in
  set { s with m } *> return r

let as_temp_reg (ll_type : T.t) (instr : I.t) : V.t t =
  let* r = make_temp_reg ll_type in
  add_instr (r <-- instr) *> return r

(* --- Type casts --- *)
(* TODO: Currently, these functions are only needed
   because bools are not supported. *)

let int_to_bool (int_v : V.t) : V.t t =
  as_temp_reg bool_t (I.trunc int_v bool_t)

let bool_to_int (bool_v : V.t) : V.t t = as_temp_reg int_t (I.zext bool_v int_t)

(* ----- Pass ----- *)

let rec pass_expr = function
  | Constant (ConstantInt i) -> return (V.i32 i)
  | Ident ident ->
      let* ptr = resolve_ident ident in
      as_temp_reg int_t (I.load ptr)
  | Binop (l, op, r) -> pass_binop l op r
  | Builtin b -> pass_builtin b

and pass_bool_expr e =
  let* int_v = pass_expr e in
  int_to_bool int_v

and pass_binop l op r =
  let make_int_binop instr = as_temp_reg int_t instr in
  let make_bool_binop instr =
    let* bool_r = as_temp_reg bool_t instr in
    bool_to_int bool_r
  in
  let* l = pass_expr l in
  let* r = pass_expr r in
  match op with
  | BinopIntPlus -> make_int_binop (I.add l r)
  | BinopIntMinus -> make_int_binop (I.sub l r)
  | BinopIntMul -> make_int_binop (I.mul l r)
  | BinopIntDiv -> make_int_binop (I.sdiv l r)
  | BinopIntMod -> make_int_binop (I.srem l r)
  | BinopIntEq -> make_bool_binop (I.eq l r)
  | BinopIntNeq -> make_bool_binop (I.ne l r)
  | BinopIntLt -> make_bool_binop (I.slt l r)
  | BinopIntLte -> make_bool_binop (I.sle l r)
  | BinopIntGt -> make_bool_binop (I.sgt l r)
  | BinopIntGte -> make_bool_binop (I.sge l r)

and pass_builtin b =
  let pass_simple_builtin ll_func expr_arg =
    let* v = pass_expr expr_arg in
    as_temp_reg int_t (I.call ll_func [ v ])
  in
  let* builtins = get_builtins_symtab in
  match b with
  | Print x -> pass_simple_builtin builtins.print x
  | Println x -> pass_simple_builtin builtins.println x
  | Assert x ->
      let* v = pass_bool_expr x in
      as_temp_reg int_t (I.call builtins.assert' [ v ])

let pass_stmt = function
  | Assign (LvalueIdent ident, expr) ->
      let* ptr = resolve_ident ident in
      let* v = pass_expr expr in
      let _, instr = I.store v ptr in
      add_instr instr
  | ExprStmt e -> pass_expr e *> return ()

let pass_terminator = function
  | Jump label ->
      let* ll_label = resolve_label label in
      add_instr (I.br1 ll_label) *> return ()
  | Return expr ->
      let* v = pass_expr expr in
      add_instr (I.ret v) *> return ()
  | CondBranch { cond = expr; if_true; if_false } ->
      let* ll_true = resolve_label if_true in
      let* ll_false = resolve_label if_false in
      let* v = pass_bool_expr expr in
      add_instr (I.br v ll_true ll_false) *> return ()

let pass_block { body; terminator; _ } =
  many body ~f:pass_stmt *> pass_terminator terminator
  *> let* s = get in
     return (s.m, List.rev s.instrs)

(* ----- Entrypoint ----- *)

let gen_block (msymtab : module_symtab) (fsymtab : func_symtab) (m : M.t)
    (ir_block : Ir.basicblock) : M.t * B.block =
  let init = { msymtab; fsymtab; m; instrs = [] } in
  let m, instructions = run_pass (pass_block ir_block) ~init in
  let ll_label = lookup_label fsymtab ir_block.label in
  let block = B.block ll_label instructions in
  (m, block)

let gen_blocks msymtab fsymtab m ir_blocks =
  List.fold_map ir_blocks ~init:m ~f:(gen_block msymtab fsymtab)
