open Base
open Ident

type symbol_table = (string, ident, String.comparator_witness) Map.t
type scope = { scope_id : int; parent : scope option; symtab : symbol_table }

type scope_manager = {
  cur_scope : scope;
  next_var_id : int;
  next_scope_id : Ident.scope_id;
}

let _empty_symbol_table = Map.empty (module String)

let empty_global =
  {
    cur_scope =
      {
        scope_id = _global_scope_id;
        parent = None;
        symtab = _empty_symbol_table;
      };
    next_var_id = 0;
    next_scope_id = _global_scope_id + 1;
  }

let enter_scope { cur_scope; next_var_id; next_scope_id } =
  let child_scope =
    {
      scope_id = next_scope_id;
      parent = Some cur_scope;
      symtab = _empty_symbol_table;
    }
  in
  { cur_scope = child_scope; next_var_id; next_scope_id = next_scope_id + 1 }

let leave_scope manager =
  match manager.cur_scope.parent with
  | None -> failwith "Cannot leave global scope"
  | Some parent -> { manager with cur_scope = parent }

let cur_scope_id manager = manager.cur_scope.scope_id

let resolve_variable { cur_scope; _ } var_name =
  let rec resolve_ident_in_scope { parent; symtab; _ } =
    match Map.find symtab var_name with
    | Some ident -> Some ident
    | None -> (
        match parent with
        | None -> None
        | Some parent -> resolve_ident_in_scope parent)
  in
  resolve_ident_in_scope cur_scope

let declare_variable manager var_name =
  let ident =
    {
      var_id = manager.next_var_id;
      scope_id = manager.cur_scope.scope_id;
      var_name;
    }
  in
  let new_symtab = Map.set manager.cur_scope.symtab ~key:var_name ~data:ident in
  let new_scope = { manager.cur_scope with symtab = new_symtab } in
  let new_manager =
    {
      manager with
      next_var_id = manager.next_var_id + 1;
      cur_scope = new_scope;
    }
  in
  (new_manager, ident)
