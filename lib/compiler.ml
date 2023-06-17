open Base
open Stdio

let load_entryfile path : Ast.ast Pass.result =
  match Fpath.of_string path with
  | Error (`Msg s) -> Error [ Err_templates.invalid_path_error s ]
  | Ok path -> (
      match Parser.parse path with
      | Error (`FileError err) ->
          Error [ Err_templates.cannot_read_file_error err ]
      | Ok ast -> ast)

let ( let* ) = Result.( >>= )

let compile path dump_ast =
  match load_entryfile path with
  | Ok ast ->
      if dump_ast then print_endline (Ast.show_ast ast)
      else print_endline "Loaded"
  | Error errs -> print_endline (Errs.show_errs errs)
