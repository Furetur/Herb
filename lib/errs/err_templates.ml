open Base
open Errs

(* - Load Errors - *)

let invalid_path_error text = err ~title:"Invalid path" ~text Loc.none_loc

let cannot_read_file_error text =
  err ~title:"Cannot read file" ~text Loc.none_loc

(* - Parsing Errors - *)

let syntax_error loc = err ~title:"Syntax error" loc

(* - Lookup Errors - *)

let no_entry_error loc =
  err ~title:"This module must have an entrypoint"
    ~text:"Define an entry point using `entry { ... }`" loc

let multiple_entry_error loc =
  err ~title:"Multiple entries"
    ~text:"The file must contain exactly one 'entry { ... }'" loc

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

let toplevel_nonliteral_expression loc =
  let title = "Top level variable must be initialized to a literal" in
  let text = "The provided value is not a literal" in
  err ~title ~text loc

(* - Type Errors - *)

let entry_wrong_return_type actual loc =
  let text =
    Printf.sprintf
      "Entry must return a value of type 'unit' or 'int' but got '%s'"
      (Typ.show_typ actual)
  in

  err ~title:"Entry return type error" ~text loc

let wrong_type expected actual loc =
  let text =
    Printf.sprintf "Expected '%s' but got '%s'" (Typ.show_typ expected)
      (Typ.show_typ actual)
  in
  err ~title:"Type error" ~text loc

let wrong_binary_op_arguments left op right loc =
  let title =
    Printf.sprintf
      "Binary operation between provided types is not defined: '%s %s %s'"
      (Typ.show_typ left)
      (Ast_operators.show_binop op)
      (Typ.show_typ right)
  in
  err ~title loc

let wrong_number_of_arguments ~expected ~actual loc =
  let title =
    Printf.sprintf "Expected %s arguments, but received %s"
      (Int.to_string expected) (Int.to_string actual)
  in
  err ~title loc

let callee_not_function actual_typ loc =
  let title =
    Printf.sprintf "Type '%s' is not callable" (Typ.show_typ actual_typ)
  in
  err ~title ~text:"Only function types can be called" loc

let if_branches_different_types then_typ else_typ loc =
  let title = "Both if branches must have equivalent types" in
  let text =
    Printf.sprintf
      "The type of 'then' branch is '%s', the type of 'else' branch is '%s'"
      (Typ.show_typ then_typ) (Typ.show_typ else_typ)
  in
  err ~title ~text loc
