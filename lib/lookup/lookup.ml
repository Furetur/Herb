open Base
open Parsetree
open Lookuptree
open Errors

type state = Scope_manager.scope_manager

module Dsl = Pass_dsl.Single_error (struct
  type t = state
end)

open Dsl

(* ----- State Helpers ----- *)

let _update_manager updater =
  let* manager = get in
  let new_manager = updater manager in
  set new_manager

let inside_scope f =
  _update_manager Scope_manager.enter_scope
  *>
  let* result = f in
  _update_manager Scope_manager.leave_scope *> return result

let resolve_variable_opt var_name =
  let* manager = get in
  return (Scope_manager.resolve_variable manager var_name)

let resolve_variable var_name =
  let* result = resolve_variable_opt var_name in
  match result with
  | Some ident -> return ident
  | None ->
      let msg = Printf.sprintf "Variable '%s' not resolved" var_name in
      fail msg

let declare_variable var_name =
  let* manager = get in
  let new_manager, ident = Scope_manager.declare_variable manager var_name in
  set new_manager *> return ident

let cur_scope_id =
  let* manager = get in
  return (Scope_manager.cur_scope_id manager)

(* ----- Pass ----- *)

let rec lookup_expr = function
  | Parsetree.Constant c -> return (Constant c)
  | Ident var_name ->
      let* ident = resolve_variable var_name in
      return (Ident ident)
  | Binop (l, op, r) ->
      let* l = lookup_expr l in
      let* r = lookup_expr r in
      return (Binop (l, op, r))
  | Call { callee; args } -> lookup_call callee args

and lookup_call callee args =
  match callee with
  | Ident name -> (
      let* builtin = lookup_builtin name args in
      match builtin with
      | Some builtin -> return (Builtin builtin)
      | None -> lookup_general_call callee args)
  | _ -> lookup_general_call callee args

and find_builtin_constructor = function
  | "print" -> Some (fun e -> Print e)
  | "println" -> Some (fun e -> Println e)
  | "assert" -> Some (fun e -> Assert e)
  | _ -> None

and lookup_builtin function_name args =
  match find_builtin_constructor function_name with
  | None -> return None
  | Some make_builtin -> (
      match args with
      | [ arg ] ->
          let* arg = lookup_expr arg in
          return (Some (make_builtin arg))
      | _ ->
          fail
            (Printf.sprintf "The %s builtin accepts only one argument"
               function_name))

and lookup_general_call callee args =
  let* callee = lookup_expr callee in
  let* args = many args ~f:lookup_expr in
  return (Call { callee; args })

let lookup_let_decl var_name expr =
  let* expr = lookup_expr expr in
  let* ident = declare_variable var_name in
  return (LetDecl (ident, expr))

let rec lookup_stmt = function
  | Parsetree.LetDecl (var_name, expr) -> lookup_let_decl var_name expr
  | Assign (l, r) ->
      let* l = lookup_expr l in
      let* r = lookup_expr r in
      return (Assign (l, r))
  | ExprStmt e ->
      let* e = lookup_expr e in
      return (ExprStmt e)
  | If (cond, then_block, else_block) ->
      let* cond = lookup_expr cond in
      let* then_block = lookup_block then_block in
      let* else_block = lookup_block else_block in
      return (If (cond, then_block, else_block))
  | While (cond, body) ->
      let* cond = lookup_expr cond in
      let* body = lookup_block body in
      return (While (cond, body))
  | Return e ->
      let* e = lookup_expr e in
      return (Return e)

and lookup_block block =
  inside_scope
    (let* scope_id = cur_scope_id in
     let* stmts = many block ~f:lookup_stmt in
     return { scope_id; stmts })

and lookup_parsetree { Parsetree.entry } =
  let* entry = lookup_block entry in
  return { entry }

let lookup (parsetree : Parsetree.parsetree) : lookuptree compilation_result =
  run_pass (lookup_parsetree parsetree) ~init:Scope_manager.empty_global
