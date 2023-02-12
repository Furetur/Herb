open Base

type herbarium_tbl = (String.t, Fpath.t, String.comparator_witness) Map.t
type cu
type proj = { root : Fpath.t; entry : cu; herbariums : herbarium_tbl }

(* Project *)
val proj_at_cwd : entry:Fpath.t -> herbarium_tbl -> proj

(* Imports *)
val resolve_rel_import : proj -> path:string list -> cu

val resolve_abs_import :
  proj ->
  herbarium:string ->
  path:string list ->
  (cu, [> `UnknownHerbarium ]) Result.t

(* CUs *)
val cu_path : cu -> Fpath.t

module Cu_comparator : sig
  type t = cu
  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.comparator
end
