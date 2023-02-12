open Base
open Stdio
open Cmdliner
open Herb


let input_file =
  let doc = "File" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let parse input_file =
  let result = In_channel.with_file input_file ~f:Parser.parse in
  match result with
  | Ok tree ->
    print_endline (Parsetree.show_herbfile tree)
  | Error (`SyntaxError loc) -> 
    let line, col = Loc.start_line_col loc in
    print_endline (Printf.sprintf "Syntax error at line %d column %d" line col)

let parse_t = Term.(const parse $ input_file)

let cmd =
  let info = Cmd.info "parse" ~doc:"Herb Parser" in
  Cmd.v info parse_t

let () = Caml.exit (Cmd.eval cmd)
