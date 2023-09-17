open Base
open Lookup_ast
open Typed_ast
open Loc
open Ident
open Typ

type state = {
  idents : (ident, typ, Ident_comparator.comparator_witness) Map.t;
}

module P = Pass.Pass (struct
  type t = state
end)

open P

(* ----- Helpers ----- *)

let require_typ ~expected ~actual loc =
  if Typ.equal_typ expected actual then return ()
  else fail (Err_templates.wrong_type expected actual loc)

let require_node_typ expected node =
  let { loc; value = { typ; _ } } = node in
  require_typ ~expected ~actual:typ loc

(* ----- State ----- *)

let set_typ ident typ =
  let* s = get in
  set { idents = Map.set s.idents ~key:ident ~data:typ }

let get_typ ident =
  let* s = get in
  match Map.find s.idents ident with
  | Some t -> return t
  | None ->
      failwith
        (Printf.sprintf "Type for identifier '%s' not found" (show_ident ident))

(* ----- Pass ----- *)

(* - Types - *)

let rec map_lookup_type { value = typ; _ } =
  match typ with
  | LTypUnit -> Unit
  | LTypString -> String
  | LTypInt -> Int
  | LTypBool -> Bool
  | LTypFun { farg_types; ret_typ } ->
      let farg_types = List.map farg_types ~f:map_lookup_type in
      let ret_typ = map_lookup_type ret_typ in
      Fun { farg_types; ret_typ }

(* - Expressions - *)

let rec check_expr { loc; value = expr } =
  match expr with
  | LLiteral lit ->
      let* typ, lit = check_literal lit in
      return { loc; value = { typ; node = TLiteral lit } }
  | LBuiltin _ -> assert false
  | LExtern { typ; linkname } ->
      let typ = map_lookup_type typ in
      return { loc; value = { typ; node = TExtern { typ; linkname } } }
  | LIdent id ->
      let* typ = get_typ id in
      return { loc; value = { typ; node = TIdent id } }
  | LAssign (l, r) -> check_assign l r loc
  | LUnOp (op, expr) -> check_unop op expr loc
  | LBinOp (l, op, r) -> check_binop l op r loc
  | LFunCall { callee; args } -> check_funcall callee args loc
  | LBlock block ->
      let* typ, block = check_block block in
      return { loc; value = { typ; node = TBlock block } }
  | LIf { cond; then_; else_ } -> check_if cond then_ else_ loc
  | LWhile { cond; body } -> check_while cond body loc

and check_block block : (Typ.typ * Typed_ast.block) t =
  let* stmts = many_seq block ~f:check_stmt in
  let typ =
    match List.last stmts with
    | None -> Unit
    | Some stmt -> (
        match stmt.value with
        | TLetStmt _ -> Unit
        | TExprStmt expr -> expr.value.typ)
  in
  return (typ, stmts)

and check_literal = function
  | LBool b -> return (Typ.Bool, TBool b)
  | LInt i -> return (Typ.Int, TInt i)
  | LString s -> return (Typ.String, TString s)
  | LFun { fargs; body } ->
      let* farg_types = check_fargs fargs in
      let fargs =
        List.map fargs ~f:(fun (id, typ) -> (id, map_lookup_type typ))
      in
      let* body = check_expr body in
      return
        (Typ.Fun { farg_types; ret_typ = body.value.typ }, TFun { fargs; body })

and check_fargs fargs =
  let check (id, typ) =
    let typ = map_lookup_type typ in
    let* _ = set_typ id typ in
    return typ
  in
  let* farg_types = many fargs ~f:check in
  return farg_types

and check_assign l r loc =
  let* l = check_expr l and* r = check_expr r in
  let* _ = require_node_typ l.value.typ r in
  return { loc; value = { typ = l.value.typ; node = TAssign (l, r) } }

and check_unop unop expr loc =
  let open Ast_operators in
  let* expr = check_expr expr in
  match unop with
  | ANot ->
      let* _ = require_node_typ Bool expr in
      return { loc; value = { typ = Bool; node = TUnOp (ANot, expr) } }

and check_binop l op r loc =
  let open Ast_operators in
  let* l = check_expr l and* r = check_expr r in
  let lt = l.value.typ in
  let rt = r.value.typ in
  let make_node typ = { loc; value = { typ; node = TBinOp (l, op, r) } } in
  let simple_operator ~left ~right ~ret =
    if Typ.equal_typ lt left && Typ.equal_typ rt right then
      return (make_node ret)
    else fail (Err_templates.wrong_binary_op_arguments lt op rt loc)
  in
  let arithmetic_operator = simple_operator ~left:Int ~right:Int ~ret:Int in
  let compatison_operator = simple_operator ~left:Int ~right:Int ~ret:Bool in
  let logic_operator = simple_operator ~left:Bool ~right:Bool ~ret:Bool in
  let plus_operator =
    match (l.value.typ, r.value.typ) with
    | Int, Int -> return (make_node Int)
    | String, String -> return (make_node String)
    | _ ->
        fail
          (Err_templates.wrong_binary_op_arguments l.value.typ op r.value.typ
             loc)
  in
  let equality_operator =
    match (lt, rt) with
    | Int, Int | Bool, Bool | String, String -> return (make_node Bool)
    | _, _ -> fail (Err_templates.wrong_binary_op_arguments lt op rt loc)
  in

  match op with
  | APlus -> plus_operator
  | AMinus | AMul | ADiv | AMod -> arithmetic_operator
  | ALt | ALte | AGte | AGt -> compatison_operator
  | AEq | ANeq -> equality_operator
  | AOr | AAnd -> logic_operator

and check_funcall callee args loc =
  let* callee = check_expr callee and* args = many args ~f:check_expr in
  match callee.value.typ with
  | Fun { farg_types; ret_typ } ->
      let* _ = check_funcall_args farg_types args loc in
      return
        { loc; value = { typ = ret_typ; node = TFunCall { callee; args } } }
  | _ -> fail (Err_templates.callee_not_function callee.value.typ loc)

and check_funcall_args farg_types args call_loc =
  match List.zip farg_types args with
  | Unequal_lengths ->
      fail
        (Err_templates.wrong_number_of_arguments
           ~expected:(List.length farg_types) ~actual:(List.length args)
           call_loc)
  | Ok zipped ->
      let* _ = many zipped ~f:(fun (typ, expr) -> require_node_typ typ expr) in
      return ()

and check_if cond then_ else_ loc =
  let* cond = check_expr cond
  and* then_typ, then_ = check_block then_
  and* else_typ, else_ = check_block else_ in
  let* _ = require_node_typ Bool cond in
  if equal_typ then_typ else_typ then
    return
      { loc; value = { typ = then_typ; node = TIf { cond; then_; else_ } } }
  else fail (Err_templates.if_branches_different_types then_typ else_typ loc)

and check_while cond body loc =
  let* cond = check_expr cond and* _, body = check_block body in
  let* _ = require_node_typ Bool cond in
  return { loc; value = { typ = Unit; node = TWhile { cond; body } } }

(* - Statements - *)

and check_stmt { loc; value = stmt } =
  match stmt with
  | LLetStmt (name, expr) ->
      let* expr = check_expr expr in
      let* _ = set_typ name expr.value.typ in
      return { loc; value = TLetStmt (name, expr) }
  | LExprStmt expr ->
      let* expr = check_expr expr in
      return { loc; value = TExprStmt expr }

(* - Top Level - *)

let check_toplevel { loc; value = id, expr } =
  let* expr = check_expr expr in
  let* _ = set_typ id expr.value.typ in
  return { loc; value = (id, expr) }

let check_entry { loc; value = entry } =
  let* typ, entry = check_block entry in
  match typ with
  | Typ.Int -> return { loc; value = entry }
  | Typ.Unit ->
      let zero_expr =
        Loc.bad_located { node = TLiteral (TInt 0); typ = Typ.Int }
      in
      let zero = { loc = Loc.bad_loc; value = TExprStmt zero_expr } in
      return { loc; value = Util.pushback entry zero }
  | _ -> fail (Err_templates.entry_wrong_return_type typ loc)

(* - Modules - *)

let check_ast { loc; value = { Lookup_ast.decls; entry } } =
  (* TODO: predeclare identifiers *)
  let* decls = many decls ~f:check_toplevel in
  let* entry = check_entry entry in
  return { loc; value = { decls; entry } }

(* Runners *)
let init = { idents = Map.empty (module Ident_comparator) }

let check (ast : lookup_ast) : typed_ast Pass.result =
  run_pass (check_ast ast) ~init
