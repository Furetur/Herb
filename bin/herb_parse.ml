open Base
open Stdio
open Cmdliner
open Herb
open Herb.Compiler

let ( let* ) = Result.( >>= )

let input_file =
  let doc = "A Herb file" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let parse input_file =
  let result =
    let* path = Util.parse_path input_file in
    let* ast = load_entryfile path in
    print_endline (Ast.show_ast ast);
    Ok ()
  in
  Errs.handle_comp_result_unit result

let parse_t = Term.(const parse $ input_file)

let cmd =
  let doc = "Herb Parser" in
  let info = Cmd.info "herb_parse" ~doc in
  Cmd.v info parse_t

let () = Stdlib.exit (Cmd.eval' cmd)
