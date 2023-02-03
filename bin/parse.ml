open Base
open Stdio
open Cmdliner

let input_file =
  let doc = "File" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let parse input_file =
  match Herb.Loader.parse input_file with
  | Ok tree ->
      let s = Herb.Parsetree.show_herbfile tree in
      print_endline s
  | Error s -> print_string s

let parse_t = Term.(const parse $ input_file)

let cmd =
  let info = Cmd.info "parse" ~doc:"Herb Parser" in
  Cmd.v info parse_t

let () = Caml.exit (Cmd.eval cmd)
