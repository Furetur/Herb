open Base

type herbarium_tbl = (String.t, Fpath.t, String.comparator_witness) Map.t
type cu = Fpath.t [@@deriving eq, show]
type proj = { root : Fpath.t; entry : cu; herbariums : herbarium_tbl }

let herb_extention = ".herb"

let cu root p =
  assert (Fpath.is_abs root);
  assert (Fpath.is_file_path p);
  let p = if Fpath.is_abs p then p else Fpath.(root // p) in
  let p = Fpath.set_ext herb_extention p in
  let p = Fpath.normalize p in
  p

let proj_at_cwd ~entry herbariums =
  let root = Fpath.v (Caml.Sys.getcwd ()) in
  assert (Fpath.is_abs root);
  let entry = cu root entry in
  { root; entry; herbariums }

(* Imports *)

let path_from_segs importpath =
  List.fold_left importpath ~init:(Fpath.v ".") ~f:Fpath.add_seg

let resolve_rel_import proj ~path =
  let path = path_from_segs path in
  cu proj.root path

let resolve_abs_import proj ~herbarium ~path =
  let path = path_from_segs path in
  match Map.find proj.herbariums herbarium with
  | Some herbpath ->
      let path = Fpath.(herbpath // path) in
      Ok (cu proj.root path)
  | None -> Error `UnknownHerbarium

let cu_path p = p
let cu_eq = Fpath.equal

module Cu_comparator = struct
  type t = cu

  include
    (val Comparator.make ~compare:Fpath.compare ~sexp_of_t:(fun x ->
             Sexp.Atom (Fpath.to_string x)))
end

type 'a cu_tbl = ((cu, 'a, Cu_comparator.comparator_witness) Map.t[@opaque])
[@@deriving show]
