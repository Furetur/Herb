open Base

type ident =
  | Global of { name : string }
  | Local of { name : string; scope : int; id : int }
[@@deriving show, eq, ord]

let global name = Global { name }
let local name ~scope ~var_id = Local { name; scope; id = var_id }
let name = function Global { name; _ } -> name | Local { name; _ } -> name

let show_ident = function
  | Global { name; _ } -> "@" ^ name
  | Local { name; scope; id } -> Printf.sprintf "%s#%d.%d" name scope id

module Ident_comparator = struct
  type t = ident

  let compare x y = Stdlib.compare x y
  let sexp_of_t x = Sexp.Atom (show_ident x)

  include (val Comparator.make ~compare ~sexp_of_t)
end
