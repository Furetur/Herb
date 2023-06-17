open Base
open Loc

type err = { loc : loc; title : string; text : string }

let err ~title ?(text = "") loc = { loc; title; text }

let show_err { loc; title; text } =
  let line, col = Loc.start_line_col loc in
  let err =
    Printf.sprintf "File '%s', line %d, column %d:\nError: %s"
      (Loc.filename loc) line col title
  in
  if String.(text = "") then err else err ^ "\n" ^ text

let show_simple_err ~title ~text =
  let head = Printf.sprintf "Error: %s" title in
  let body = if String.(text = "") then "" else "\n\n" ^ text in
  head ^ body

let show_errs errs = String.concat ~sep:"\n\n" (List.map errs ~f:show_err)
