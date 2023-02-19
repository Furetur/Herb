open Base
open Loc
open Parsetree
open Ast

type lowering_state = {
  cu : Proj.cu;
  top_decls : Ast.top_decl list;
  entry : Ast.entry option;
}

module P = Passes.Pass (struct
  type t = lowering_state
end)

open P

(* ----- Combinators ------ *)

let add_top_decl d =
  let* s = access in
  put { s with top_decls = d :: s.top_decls }

let set_entry entry =
  let* s = access in
  match s.entry with
  | Some _ ->
      add_err
        {
          cu = s.cu;
          loc = entry.loc;
          kind = SyntaxError;
          title = "Entry already defined";
          text = "";
        }
  | None -> put { s with entry = Some entry }

(* ----- Pass ----- *)

let map_located { Loc.loc; Loc.value } f =
  let* v = f value in
  return { Loc.loc; Loc.value = v }

(* --- Types --- *)

let rec lower_type t =
  map_located t @@ function
  | PTypNamed n -> return (ATypNamed n)
  | PTypFun { arg_types; ret_typ } ->
      let* farg_types = many arg_types ~f:lower_type in
      let* ret_type = lower_type ret_typ in
      return (ATypFun { farg_types; ret_type })

(* --- Expression --- *)

let true_ = Loc.bad_located (ALiteral (ABool true))
let false_ = Loc.bad_located (ALiteral (ABool false))
let expr_stmt e = { Loc.loc = e.Loc.loc; Loc.value = AExprStmt e.Loc.value }

let rec lower_raw_expr = function
  | PInt i -> return (ALiteral (AInt i))
  | PString s -> return (ALiteral (AString s))
  | PFunLiteral f -> lower_fun_literal f
  | PIdent s -> return (AIdent s)
  | PAssign (l, r) ->
      let* l = lower_expr l in
      let* r = lower_expr r in
      return (AAssign (l, r))
  | PUnOp (op, expr) -> lower_unop op expr
  | PBinOp (l, op, r) -> lower_binop l op r
  | PFunCall { callee; args } ->
      let* callee = lower_expr callee in
      let* args = many args ~f:lower_expr in
      return (AFunCall { callee; args })
  | PBlock b ->
      let* b = lower_block b in
      return (ABlock b)
  | PIf { cond; then_; else_ } ->
      let* cond = lower_expr cond in
      let* then_ = lower_block then_ in
      let* else_ = lower_block else_ in
      return (AIf { cond; then_; else_ })
  | PWhile { cond; body } ->
      let* cond = lower_expr cond in
      let* body = lower_block body in
      return (AWhile { cond; body })
  | PFor { i; start_; end_; body } -> lower_for i start_ end_ body

and lower_expr e = map_located e lower_raw_expr

and lower_fun_literal { formal_args; body } =
  let lower_farg (name, t) =
    let* t = lower_type t in
    return (name, t)
  in
  let* fargs = many formal_args ~f:lower_farg in
  let* body = lower_expr body in
  return (ALiteral (AFun { fargs; body }))

and lower_unop op expr =
  let* expr = lower_expr expr in
  match op with PNot -> return (AUnOp (ANot, expr))

and lower_binop l op r =
  let* l = lower_expr l in
  let* r = lower_expr r in
  let regular_binop op = return (ABinOp (l, op, r)) in
  match op with
  | PAnd ->
      return
        (AIf { cond = l; then_ = [ expr_stmt r ]; else_ = [ expr_stmt false_ ] })
  | POr ->
      return
        (AIf { cond = l; then_ = [ expr_stmt true_ ]; else_ = [ expr_stmt r ] })
  | PPlus -> regular_binop APlus
  | PMinus -> regular_binop AMinus
  | PMul -> regular_binop AMul
  | PDiv -> regular_binop ADiv
  | PMod -> regular_binop AMod
  | PLt -> regular_binop ALt
  | PLte -> regular_binop ALte
  | PEq -> regular_binop AEq
  | PNeq -> regular_binop ANeq
  | PGt -> regular_binop AGt
  | PGte -> regular_binop AGte

and lower_for i start_ end_ body =
  let* start_ = lower_expr start_ in
  let* end_ = lower_expr end_ in
  let* body = lower_block body in
  (* Ast *)
  let let_counter = Loc.bad_located (ALetStmt (i, start_)) in
  let i_ident = bad_located (AIdent i) in
  let one = bad_located (ALiteral (AInt 1)) in
  let increment_counter =
    bad_located (AAssign (i_ident, bad_located (ABinOp (i_ident, APlus, one))))
  in
  let while_body =
    [ expr_stmt (bad_located (ABlock body)); expr_stmt increment_counter ]
  in
  let cond = bad_located (ABinOp (i_ident, ALte, end_)) in
  let while_ = bad_located (AWhile { cond; body = while_body }) in
  return (ABlock [ let_counter; expr_stmt while_ ])

and lower_block b = many b ~f:lower_stmt

(* ----- Statements ----- *)

and lower_stmt s =
  map_located s @@ function
  | PLet (name, expr) ->
      let* expr = lower_expr expr in
      return (ALetStmt (name, expr))
  | PExprStmt expr ->
      let* expr = lower_raw_expr expr in
      return (AExprStmt expr)

(* ----- Top level ----- *)

let lower_toplevel { loc; value } =
  match value with
  | PEntry b ->
      let* b = lower_block b in
      set_entry { loc; value = b }
  | PToplevelLet (name, expr) ->
      let* expr = lower_expr expr in
      add_top_decl { loc; value = AToplevelLet (name, expr) }
  | PExtern { name; typ; linkname } ->
      let* typ = lower_type typ in
      add_top_decl { loc; value = AExtern { name; typ; linkname } }

(* ----- File ----- *)

let lower_file { Parsetree.decls; _ } = fold_state decls ~f:lower_toplevel

open Loader

let init cu = { cu; top_decls = []; entry = None }

let lower_lib_module { cu; imports; tree } =
  let st, err, _ = run_pass (lower_file tree) ~init:(init cu) in
  match err with
  | [] -> Ok { cu; imports; decls = st.top_decls }
  | errs -> Error errs

let lower_entry_module { cu; imports; tree } =
  let st, err, _ = run_pass (lower_file tree) ~init:(init cu) in
  match err with
  | [] -> (
      match st.entry with
      | Some entry ->
          let lib = { cu; imports; decls = st.top_decls } in
          Ok { module_ = lib; entry }
      | None ->
          let open Errs in
          let err =
            {
              cu;
              loc = start_loc;
              kind = SyntaxError;
              title = "This module must have an entrypoint";
              text = "Define an entry point using `entry {}`";
            }
          in
          Error [ err ])
  | errs -> Error errs

let combine xs = Result.map_error (Result.combine_errors xs) ~f:List.concat

let lower { lib_modules; entry_module } =
  let ( let* ) = Result.( >>= ) in
  let* libs = combine (List.map lib_modules ~f:lower_lib_module) in
  let* entry = lower_entry_module entry_module in
  Ok { Ast.libs; Ast.entry }
