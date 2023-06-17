open Errs

(* - Load errors - *)

let invalid_path_error text = err ~title:"Invalid path" ~text Loc.none_loc

let cannot_read_file_error text =
  err ~title:"Cannot read file" ~text Loc.none_loc

(* - Parsing errors - *)

let syntax_error loc = err ~title:"Syntax error" loc

(* - Lookup errors - *)

let no_entry_error file =
  err ~title:"This module must have an entrypoint"
    ~text:"Define an entry point using `entry { ... }`" (Loc.infile_loc file)

let multiple_entry_error last_entry_loc =
  err ~title:"Entry redeclaration"
    ~text:"The project entry file must contain exactly one 'entry { ... }'"
    last_entry_loc

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
