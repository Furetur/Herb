open Base
open Loc
open Ast
open Lookup_ast

type state = { tree : Scopetree.t }

module P = Pass.Pass (struct
  type t = state
end)

open P

(* ----- Helper ----- *)

let located (f : loc -> 'a -> 'b t) : 'a located -> 'b t =
 fun { value; loc } -> f loc value

(* ----- State ----- *)

let enter_scope =
  let* s = get in
  set { tree = Scopetree.enter_scope s.tree }

let leave_scope =
  let* s = get in
  set { tree = Scopetree.leave_scope s.tree }

let scope t =
  let* _ = enter_scope in
  let* tt = t in
  let* _ = leave_scope in
  return tt

let declare_new_binding name decl_loc =
  let* s = get in
  match Scopetree.declare_new_name s.tree name with
  | Ok (id, tree) ->
      let* _ = set { tree } in
      return id
  | Error _ -> fail (Err_templates.toplevel_redeclaration name decl_loc)

let try_resolve name =
  let* s = get in
  return (Scopetree.resolve_name s.tree name)

let resolve name loc =
  let* ident = try_resolve name in
  match ident with
  | Some ident -> return ident
  | None -> fail (Err_templates.undefined_variable name loc)

let resolve_named_type loc = function
  | "unit" -> return { loc; value = LTypUnit }
  | "string" -> return { loc; value = LTypString }
  | "int" -> return { loc; value = LTypInt }
  | "bool" -> return { loc; value = LTypBool }
  | name -> fail (Err_templates.undefined_type name loc)

(* ----- Pass ----- *)

(* - Types - *)

let rec lookup_typ { loc; value = typ } =
  match typ with
  | ATypNamed name -> resolve_named_type loc name
  | ATypFun { farg_types; ret_typ } ->
      let* farg_types = many farg_types ~f:lookup_typ
      and* ret_typ = lookup_typ ret_typ in
      return { loc; value = LTypFun { farg_types; ret_typ } }

(* - Expressions - *)

let rec lookup_expr { loc; value = expr } =
  match expr with
  | ALiteral lit ->
      let* lit = lookup_literal loc lit in
      return { loc; value = LLiteral lit }
  | AIdent name ->
      let* id = resolve name loc in
      return { loc; value = LIdent id }
  | AAssign (l, r) ->
      let* l = lookup_expr l and* r = lookup_expr r in
      return { loc; value = LAssign (l, r) }
  | AUnOp (op, expr) ->
      let* expr = lookup_expr expr in
      return { loc; value = LUnOp (op, expr) }
  | ABinOp (l, op, r) ->
      let* l = lookup_expr l and* r = lookup_expr r in
      return { loc; value = LBinOp (l, op, r) }
  | AFunCall { callee; args } ->
      let* callee = lookup_expr callee and* args = many args ~f:lookup_expr in
      return { loc; value = LFunCall { callee; args } }
  | ABlock x ->
      let* block = lookup_block x in
      return { loc; value = LBlock block }
  | AIf { cond; then_; else_ } -> lookup_if loc cond then_ else_
  | AWhile { cond; body } -> lookup_while loc cond body

and lookup_block block = scope (many_seq block ~f:lookup_stmt)

and lookup_farg loc (name, typ) =
  let* id = declare_new_binding name loc in
  let* typ = lookup_typ typ in
  return (id, typ)

and lookup_literal loc = function
  | ABool x -> return (LBool x)
  | AInt x -> return (LInt x)
  | AString x -> return (LString x)
  | AFun { fargs; body } ->
      let* funlit =
        scope
          (let* fargs = many fargs ~f:(lookup_farg loc) in
           let* body = lookup_expr body in
           return (LFun { fargs; body }))
      in
      return funlit

and lookup_if loc cond then_ else_ =
  let* cond = lookup_expr cond
  and* then_ = lookup_block then_
  and* else_ = lookup_block else_ in
  return { loc; value = LIf { cond; then_; else_ } }

and lookup_while loc cond body =
  let* cond = lookup_expr cond and* body = lookup_block body in
  return { loc; value = LWhile { cond; body } }

(* - Statements - *)

and lookup_stmt { loc; value = stmt } =
  match stmt with
  | ALetStmt (name, expr) ->
      let* _ = get in
      Caml.print_endline ("visiting let " ^ name);
      let* expr = lookup_expr expr in
      let* id = declare_new_binding name loc in
      return { loc; value = LLetStmt (id, expr) }
  | AExprStmt expr ->
      let* _ = get in
      Caml.print_endline ("visiting expr stmt: " ^ Ast.show_expr expr);
      let* expr = lookup_expr expr in
      return { loc; value = LExprStmt expr }

(* - Top Level - *)

let lookup_top_extern { loc; value = { name; typ; linkname; _ } } =
  let* ident = declare_new_binding name loc and* typ = lookup_typ typ in
  let expr = { loc; value = LExtern { typ; linkname } } in
  return { loc; value = (ident, expr) }

let lookup_predeclare_top_letdecl { loc; value = id, expr } =
  let* ident = declare_new_binding id loc in
  return { loc; value = (ident, expr) }

let lookup_top_letdecl_expressions { loc; value = ident, expr } =
  let* expr = lookup_expr expr in
  return { loc; value = (ident, expr) }

let lookup_entry { loc; value = block } =
  let* block = lookup_block block in
  return { loc; value = block }

(* - Modules - *)

let lookup_ast ({ loc; value = { decls } } : ast) : lookup_ast t =
  (* Extract *)
  let aux_extract_decls f =
    List.filter_map decls ~f:(fun { loc; value } ->
        match f value with
        | Some new_value -> Some { loc; value = new_value }
        | None -> None)
  in
  let externs =
    aux_extract_decls (function AExtern x -> Some x | _ -> None)
  in
  let letdecls =
    aux_extract_decls (function AToplevelLet x -> Some x | _ -> None)
  in
  let entries = aux_extract_decls (function AEntry x -> Some x | _ -> None) in

  (* Lookup externs  *)
  let* externs = many externs ~f:lookup_top_extern in

  (* Predeclare and then lookup let declarations *)
  let* letdecls = many letdecls ~f:lookup_predeclare_top_letdecl in
  let* letdecls = many letdecls ~f:lookup_top_letdecl_expressions in

  (* Lookup entry *)
  let* entry =
    match entries with
    | [] -> fail (Err_templates.no_entry_error loc)
    | [ entry ] -> lookup_entry entry
    | _ -> fail (Err_templates.multiple_entry_error loc)
  in

  (* Return lookup_ast *)
  let decls = List.append externs letdecls in
  return { loc; value = { decls; entry } }

(* - Runners - *)

let init = { tree = Scopetree.empty }

let lookup (ast : ast) : lookup_ast Pass.result =
  run_pass (lookup_ast ast) ~init
