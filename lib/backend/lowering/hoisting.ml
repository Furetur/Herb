open Base
open Typed_ast
open Loc

let rec hoist_from_stmt { value = stmt; _ } =
  match stmt with
  | TLetStmt (ident, e) -> (ident, e.value.typ) :: hoist_from_expr e
  | TExprStmt expr -> hoist_from_expr expr

and hoist_from_stmts stmts = List.concat_map ~f:hoist_from_stmt stmts

and hoist_from_expr { value = { node = expr; _ }; _ } =
  match expr with
  | TBinOp (e1, _, e2) -> hoist_from_exprs [ e1; e2 ]
  | TUnOp (_, e) | TAssign (_, e) -> hoist_from_expr e
  | TFunCall { callee; args } -> hoist_from_exprs (callee :: args)
  | TBlock block -> hoist_from_stmts block
  | TIf { cond; then_; else_ } ->
      hoist_from_expr cond @ hoist_from_stmts then_ @ hoist_from_stmts else_
  | TWhile { cond; body } -> hoist_from_expr cond @ hoist_from_stmts body
  | _ -> []

and hoist_from_exprs exprs = List.concat_map ~f:hoist_from_expr exprs

(* Get all local variables *)
let hoist (block : block) : Lowered_tree.typed_ident list =
  hoist_from_stmts block
