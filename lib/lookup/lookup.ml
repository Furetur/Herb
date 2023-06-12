open Base
open Loc
open Proj
open Ident
open Ast
open Lookup_ast

type scope_kind = Global of cu | Local of { parent : scope }

and scope = {
  id : int;
  kind : scope_kind;
  tbl : (string, ident, String.comparator_witness) Map.t;
  next_var_id : int;
}

type state = {
  looked_up_modules : (cu, lib_module, Cu_comparator.comparator_witness) Map.t;
  scope : scope;
  next_scope_id : int;
}

module P = Pass.Pass (struct
  type t = state
end)

open P

(* ----- Helper ----- *)

let empty_name_tbl = Map.empty (module String)

let located (f : loc -> 'a -> 'b t) : 'a located -> 'b t =
 fun { value; loc } -> f loc value

(* ----- State ----- *)

let enter_scope =
  let* s = access in
  let scope =
    {
      id = s.next_scope_id;
      kind = Local { parent = s.scope };
      tbl = empty_name_tbl;
      next_var_id = 0;
    }
  in
  put { s with scope; next_scope_id = s.next_scope_id + 1 } *> return s.scope

let set_scope scope =
  let* s = access in
  put { s with scope }

let leave_scope = set_scope

let declare_new_binding name decl_loc =
  let* s = access in
  let scope = s.scope in

  match scope.kind with
  | Local _ ->
      let ident = Ident.local name ~scope:scope.id ~var_id:scope.next_var_id in
      let tbl = Map.set scope.tbl ~key:name ~data:ident in
      let scope = { scope with tbl; next_var_id = scope.next_var_id + 1 } in
      put { s with scope } *> return ident
  | Global cu -> (
      match Map.find scope.tbl name with
      | Some _ -> fail (Errs.toplevel_redeclaration name decl_loc)
      | None ->
          let ident = Ident.global name cu in
          let scope =
            { scope with tbl = Map.set scope.tbl ~key:name ~data:ident }
          in
          put { s with scope } *> return ident)

let add_ident_to_scope ident =
  let* s = access in
  let tbl = Map.set s.scope.tbl ~key:(Ident.name ident) ~data:ident in
  let scope = { s.scope with tbl } in
  put { s with scope }

let try_resolve name =
  let rec resolve_in_scope scope =
    match Map.find scope.tbl name with
    | Some id -> Some id
    | None -> (
        match scope.kind with
        | Local { parent } -> resolve_in_scope parent
        | Global _ -> None)
  in
  let* { scope; _ } = access in
  return (resolve_in_scope scope)

let resolve name loc =
  let* ident = try_resolve name in
  match ident with
  | Some ident -> return ident
  | None -> fail (Errs.undefined_variable name loc)

let import_all { imported_cu = cu; _ } =
  let declare { value = ident, _; _ } = add_ident_to_scope ident in

  let* { looked_up_modules; _ } = access in
  match Map.find looked_up_modules cu with
  | None ->
      failwith
        (Printf.sprintf "Module '%s' is supposed to have been looked up"
           (show_cu cu))
  | Some lib -> many lib ~f:declare *> return ()

(* ----- Pass ----- *)

(* - Types - *)

let rec lookup_named_typ loc = function
  | "string" -> return { loc; value = LTypString }
  | "int" -> return { loc; value = LTypInt }
  | "bool" -> return { loc; value = LTypBool }
  | name -> fail (Errs.undefined_type name loc)

and lookup_typ { loc; value = typ } =
  match typ with
  | ATypNamed name -> lookup_named_typ loc name
  | ATypFun { farg_types; ret_typ } ->
      let* farg_types = many farg_types ~f:lookup_typ
      and* ret_typ = lookup_typ ret_typ in
      return { loc; value = LTypFun { farg_types; ret_typ } }

(* - Expressions - *)

let lookup_expr expr = failwith "todo"
let lookup_block block = failwith "todo"

(* - Top Level - *)

let lookup_top_extern { loc; value = { name; typ; linkname; _ } } =
  let* ident = declare_new_binding name loc and* typ = lookup_typ typ in
  let expr = { loc; value = LExtern { typ; linkname } } in
  return { loc; value = (ident, expr) }

let lookup_top_letdecl { loc; value = ident, expr } =
  let* expr = lookup_expr expr in
  return { loc; value = (ident, expr) }

(* - Sort - *)
let sort_top_letdecls x = return x

(* - Modules - *)

let lookup_lib_module { resolved_imports; decls; cu } =
  let* _ = many resolved_imports ~f:import_all in

  let externs =
    List.filter_map decls ~f:(function
      | { loc; value = AExtern x } -> Some { loc; value = x }
      | _ -> None)
  in
  let* externs = many externs ~f:lookup_top_extern in

  let letdecls =
    List.filter_map decls ~f:(function
      | { loc; value = AToplevelLet x } -> Some { loc; value = x }
      | _ -> None)
  in
  let predeclare_let { loc; value = name, expr } =
    let* ident = declare_new_binding name loc in
    return { loc; value = (ident, expr) }
  in
  let* letdecls = many letdecls ~f:predeclare_let in
  let* letdecls = many letdecls ~f:lookup_top_letdecl in
  let* letdecls = sort_top_letdecls letdecls in

  return { cu; decls = List.append externs letdecls }

let lookup_entry_module m =
  let* lib = lookup_lib_module m in

  let entries =
    List.filter_map m.decls ~f:(function
      | { loc; value = AEntry x } -> Some { loc; value = x }
      | _ -> None)
  in
  match entries with
  | [] -> fail (Errs.no_entry_error m.cu)
  | [ entry ] ->
      let* entry = lookup_block entry in
      return { module_ = lib; entry }
  | entry :: _ -> fail (Errs.multiple_entry_error entry.loc)

(* - Runners - *)

let init cu mods =
  match List.map mods ~f:(fun m -> (m.cu, m)) |> Map.of_alist (module Cu_comparator) with
  | `Ok looked_up_modules -> 
  {
    looked_up_modules = looked_up_modules;
    scope = { id = 0; kind = Global cu; next_var_id = 0; tbl = empty_name_tbl };
    next_scope_id = 1;
  }
  | `Duplicate_key cu -> failwith (Printf.sprintf "Module '%s' occurs multiple times" (show_cu cu))


let lookup { Ast.entry_module; lib_modules } =
  let ( let* ) = Result.(>>=) in
  let rec lookup_libs acc = function
  | [] -> Ok (List.rev acc)
  | lib::libs -> 
    let res = run_pass (lookup_lib_module lib) ~init:(init lib.cu acc) in
    match res with
    | Ok lib -> lookup_libs (lib::acc) libs
    | Error errs -> Error errs
  in

  let* libs = lookup_libs [] lib_modules in
  let* entry = run_pass (lookup_entry_module entry_module) ~init:(init entry_module.cu libs) in
  Ok  { entry_module = entry; lib_modules = libs }
