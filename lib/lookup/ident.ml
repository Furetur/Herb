open Base

type scope_id = int [@@deriving show, eq, ord]

type ident = { scope_id : scope_id; var_name : string; var_id : int }
[@@deriving eq, ord]

let pp_ident fmt { scope_id; var_name; var_id } =
  Stdlib.Format.fprintf fmt "%s_%d_%d" var_name scope_id var_id

let show_ident ident = Stdlib.Format.asprintf "%a" pp_ident ident

let mangle ident =
  Printf.sprintf "%s_%d_%d" ident.var_name ident.scope_id ident.var_id

let _global_scope_id = 0

let local_ident var_name ~scope_id ~var_id =
  assert (scope_id <> 0);
  { var_name; scope_id; var_id }

let global_ident var_name ~var_id =
  { scope_id = _global_scope_id; var_name; var_id }

let is_global { scope_id; _ } = scope_id <> _global_scope_id
let is_local ident = not (is_global ident)
let name { var_name; _ } = var_name

module Comparator = struct
  type t = ident

  let compare = compare_ident
  let sexp_of_t _ = failwith "Not implemented"

  include (val Base.Comparator.make ~compare ~sexp_of_t)
end
