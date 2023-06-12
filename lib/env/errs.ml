open Base
open Loc
open Proj

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

(* ----- Error Templates ----- *)

let syntax_error loc = err ~title:"Syntax error" loc

(* - Loader errors - *)

let could_not_open_entryfile path text =
  err ~title:"Could not open entry file" ~text (Loc.infile_loc path)

let unknown_herbarium name loc =
  let title = Printf.sprintf "Unknown herbarium '%s'" name in
  err ~title loc

let could_not_read_file_error text importloc =
  err ~title:"Could not read file" ~text importloc

let dependency_cycle_error cycle =
  let text =
    String.concat ~sep:"->\n  " (List.map cycle ~f:(fun cu -> show_cu cu))
  in
  err ~title:"Dependency cycle detected" ~text Loc.start_loc

(* - Lookup errors - *)

let no_entry_error cu =
  err ~title:"This module must have an entrypoint"
    ~text:"Define an entry point using `entry { ... }`" (Loc.incu_loc cu)

let multiple_entry_error last_entry_loc =
  err ~title:"Entry redeclaration" ~text:"The project entry file must contain exactly one 'entry { ... }'" last_entry_loc

let undefined_variable name loc =
  let title = Printf.sprintf "Unknown variable '%s'" name in
  err ~title loc

let undefined_type name loc =
  let title = Printf.sprintf "Unknown type '%s'" name in
  let text = "Valid named types are string, int and bool" in
  err ~title ~text loc

let toplevel_redeclaration name loc =
  let title = Printf.sprintf "Variable redeclaration '%s'" name in
  let text = "Top level variables must have unique names" in
  err ~title ~text loc