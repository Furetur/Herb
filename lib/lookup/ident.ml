open Base
open Proj

type ident =
  | Global of { name : string; cu : cu }
  | Local of { name : string; scope : int; id : int }
[@@derive show, eq, ord]

let show_ident = function
  | Global { name; _ } -> "@" ^ name
  | Local { name; scope; id } -> Printf.sprintf "%s#%d.%d" name scope id

module Ident_comparator = struct
  type t = ident

  let compare x y = Caml.compare x y
  let sexp_of_t x = Sexp.Atom (show_ident x)

  include (val Comparator.make ~compare ~sexp_of_t)
end
