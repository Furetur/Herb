open Base
open Stdio
open Cmdliner
open Herb

let input_file =
  let doc = "File" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let parse input_file =
  match Fpath.of_string input_file with
  | Error (`Msg msg) -> print_endline ("Invalid path: " ^ msg)
  | Ok path -> (
      match Parser.parse ~errpath:path path with
      | Ok tree -> print_endline (Ast.show_parsed_file tree)
      | Error (`SyntaxError loc) ->
          let line, col = Loc.start_line_col loc in
          print_endline
            (Printf.sprintf "Syntax error at line %d column %d" line col)
      | Error (`FileError msg) -> print_endline ("Error: " ^ msg))

let parse_t = Term.(const parse $ input_file)

let cmd =
  let info = Cmd.info "parse" ~doc:"Herb Parser" in
  Cmd.v info parse_t

let () = Caml.exit (Cmd.eval cmd)
