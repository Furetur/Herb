open Base
open Stdio

let herbariums = Map.empty (module String)

let compile path =
  match Fpath.of_string path with
  | Error (`Msg s) -> print_endline ("Invalid path:" ^ s)
  | Ok entry -> (
      let proj = Proj.proj_at_cwd ~entry herbariums in
      match Loader.load_project proj with
      | Ok ast ->
          print_endline (Ast.show_ast ast);
          print_endline "Loaded"
      | Error (`EntryFileError (_, s)) ->
          print_endline
            Errs.(show_simple_err ~title:"Could not open file" ~text:s)
      | Error (`Errs errs) -> print_endline (Errs.show_errs errs))
