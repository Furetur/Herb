open Base

type label = { name : string; id : int } [@@deriving ord]

let show_label { name; id } = Printf.sprintf ".%s_%d" name id

module Generator = struct
  type t = { component_name : string; next_id : int }

  let make component_name = { component_name; next_id = 0 }

  let make_label { component_name; next_id } ~name =
    let label = { name = component_name ^ "$" ^ name; id = next_id } in
    ({ component_name; next_id = next_id + 1 }, label)

  let make_temp_label t = make_label t ~name:"temp"
end

module Comparator = struct
  type t = label

  let compare = compare_label

  let sexp_of_t t =
    Base.Sexp.List
      [ Base.Sexp.Atom t.name; Base.Sexp.Atom (Int.to_string t.id) ]

  include (val Base.Comparator.make ~compare:compare_label ~sexp_of_t)
end
