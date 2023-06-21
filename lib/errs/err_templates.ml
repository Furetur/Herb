open Errs

(* - Load errors - *)

let invalid_path_error text = err ~title:"Invalid path" ~text Loc.none_loc

let cannot_read_file_error text =
  err ~title:"Cannot read file" ~text Loc.none_loc

(* - Parsing errors - *)

let syntax_error loc = err ~title:"Syntax error" loc

(* - Lookup errors - *)

let no_entry_error loc =
  err ~title:"This module must have an entrypoint"
    ~text:"Define an entry point using `entry { ... }`" loc

let multiple_entry_error loc =
  err ~title:"Multiple entries"
    ~text:"The file must contain exactly one 'entry { ... }'"
    loc

let undefined_variable name loc =
  let title = Printf.sprintf "Undefined variable name '%s'" name in
  err ~title loc

let undefined_type name loc =
  let title = Printf.sprintf "Unknown type '%s'" name in
  let text = "Valid named types are unit, string, int and bool" in
  err ~title ~text loc

let toplevel_redeclaration name loc =
  let title = Printf.sprintf "Name redeclaration '%s'" name in
  let text = "Top level names must be unique" in
  err ~title ~text loc
