open Base
open Ident

type scope_kind = Global | Local of { parent : scope }

and scope = {
  id : int;
  kind : scope_kind;
  tbl : (string, ident, String.comparator_witness) Map.t;
}

type t = { next_scope_id : int; next_ident_id : int; cur_scope : scope }

let empty =
  {
    next_scope_id = 1;
    next_ident_id = 0;
    cur_scope = { id = 1; kind = Global; tbl = Map.empty (module String) };
  }

let enter_scope t =
  let new_scope =
    {
      id = t.next_scope_id;
      kind = Local { parent = t.cur_scope };
      tbl = Map.empty (module String);
    }
  in
  { t with next_scope_id = t.next_scope_id + 1; cur_scope = new_scope }

let leave_scope t =
  match t.cur_scope.kind with
  | Global -> failwith "Cannot leave global scope"
  | Local { parent } -> { t with cur_scope = parent }

let declare_new_name t name =
  let declare_global () =
    match Map.find t.cur_scope.tbl name with
    | Some x -> Error x
    | None ->
        let ident = Ident.Global { name } in
        let tbl = Map.set t.cur_scope.tbl ~key:name ~data:ident in
        Ok (ident, { t with cur_scope = { t.cur_scope with tbl } })
  in
  let declare_local () =
    let ident =
      Ident.Local { name; id = t.next_ident_id; scope = t.cur_scope.id }
    in
    let tbl = Map.set t.cur_scope.tbl ~key:name ~data:ident in
    ( ident,
      {
        t with
        next_ident_id = t.next_ident_id + 1;
        cur_scope = { t.cur_scope with tbl };
      } )
  in
  match t.cur_scope.kind with
  | Global -> declare_global ()
  | Local _ -> Ok (declare_local ())

let resolve_name t name =
  let rec aux scope =
    match Map.find scope.tbl name with
    | Some id -> Some id
    | None -> (
        match scope.kind with Local { parent } -> aux parent | Global -> None)
  in
  aux t.cur_scope
