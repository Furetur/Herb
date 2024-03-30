open Stdio
open Errors
open Compiler_options
open Pass_dsl.Result_dsl_syntax

type ('input, 'output) pass = 'input -> 'output compilation_result

let ( $ ) (pass1 : _ pass) (pass2 : _ pass) : _ pass =
 fun x ->
  let* y = pass1 x in
  pass2 y

let make_print_pass to_string_converter x =
  print_endline (to_string_converter x);
  return ()

let setup_passes { ll_outpath; bin_outpath; expected_compilation_result; _ } =
  let parse = Parser.parse in
  let print_parsetree = make_print_pass Parsetree.show_parsetree in
  let lookup = Lookup.lookup in
  let print_lookuptree = make_print_pass Lookuptree.show_lookuptree in
  let lower = Lowering.lower in
  let print_ir = make_print_pass Ir_prettyprint.show_ir in
  let gen = Ll_gen.gen in

  match expected_compilation_result with
  | Parsetree -> parse $ print_parsetree
  | Lookuptree -> parse $ lookup $ print_lookuptree
  | Ir -> parse $ lookup $ lower $ print_ir
  | Binary -> parse $ lookup $ lower $ gen ll_outpath bin_outpath

let compile (options : compiler_options) =
  let passes = setup_passes options in
  passes options.inpath
