open Base
open Loc
open Proj

type err = { cu : cu; loc : loc; title : string; text : string }

let err ~title ?(text = "") loc cu = { cu; loc; title; text }

let show_err { cu; loc; title; text } =
  let path = Fpath.to_string (cu_path cu) in
  let line, col = Loc.start_line_col loc in
  let err =
    Printf.sprintf "File '%s', line %d, column %d:\nError: %s" path line col
      title
  in
  if String.(text = "") then err else err ^ "\n" ^ text

let show_simple_err ~title ~text =
  let head = Printf.sprintf "Error: %s" title in
  let body = if String.(text = "") then "" else "\n\n" ^ text in
  head ^ body

let show_errs errs = String.concat ~sep:"\n\n" (List.map errs ~f:show_err)

module Templates = struct
  type t = cu -> err

  let syntax_error loc : t = err ~title:"Syntax error" loc

  (* Loader errors *)

  let dependency_cycle_error cycle : t =
    let text =
      String.concat ~sep:"->\n  " (List.map cycle ~f:(fun cu -> show_cu cu))
    in
    err ~title:"Dependency cycle detected" ~text Loc.start_loc

  (* Lookup errors *)

  let no_entry_error : t =
    err ~title:"This module must have an entrypoint"
      ~text:"Define an entry point using `entry {}`" Loc.start_loc
end
