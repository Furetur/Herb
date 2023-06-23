open Base

type typ =
  | Unit
  | Int
  | String
  | Bool
  | Fun of { farg_types : typ list; ret_typ : typ }
[@@deriving show, eq]

type 'a typed = { typ : typ; node : 'a } [@@deriving show]

(* ===== Pretty printing ===== *)

let rec show_typ = function
  | Unit -> "unit"
  | Int -> "int"
  | String -> "string"
  | Bool -> "bool"
  | Fun { farg_types; ret_typ } ->
      let fargs = List.map farg_types ~f:show_typ |> String.concat ~sep:", " in
      let ret = show_typ ret_typ in
      Printf.sprintf "(%s) -> %s" fargs ret
