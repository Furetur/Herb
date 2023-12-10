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

module ThisPass = Passes.NoErrors (struct
  type t = state
end)

open ThisPass

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

(* ----- Pass ----- *)

let rec pass_expr = function
  | Constant (ConstantInt i) -> return (V.i32 i)
  | Ident ident ->
      let* ptr = resolve_ident ident in
      as_temp_reg int_t (I.load ptr)
  | Binop (l, op, r) -> (
      let* l = pass_expr l in
      let* r = pass_expr r in
      match op with
      | BinopIntPlus -> as_temp_reg int_t (I.add l r)
      | BinopIntMinus -> as_temp_reg int_t (I.sub l r)
      | BinopIntMul -> as_temp_reg int_t (I.mul l r)
      | BinopIntDiv -> as_temp_reg int_t (I.sdiv l r)
      | BinopIntMod -> as_temp_reg int_t (I.srem l r)
      | BinopIntEq -> as_temp_reg bool_t (I.eq l r)
      | BinopIntNeq -> as_temp_reg bool_t (I.ne l r)
      | BinopIntLt -> as_temp_reg bool_t (I.slt l r)
      | BinopIntLte -> as_temp_reg bool_t (I.sle l r)
      | BinopIntGt -> as_temp_reg bool_t (I.sgt l r)
      | BinopIntGte -> as_temp_reg bool_t (I.sge l r))
  | Builtin b -> pass_builtin b

and pass_builtin b =
  let pass_simple_builtin ll_func expr_arg =
    let* v = pass_expr expr_arg in
    as_temp_reg int_t (I.call ll_func [ v ])
  in
  let* builtins = get_builtins_symtab in
  match b with
  | Print x -> pass_simple_builtin builtins.print x
  | Println x -> pass_simple_builtin builtins.println x
  | Assert x -> pass_simple_builtin builtins.assert' x

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
      let* v = pass_expr expr in
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
