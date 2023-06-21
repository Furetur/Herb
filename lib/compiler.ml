open Base
open Stdio

(* - Helpers - *)

let load_entryfile path : Ast.ast Pass.result =
  match Fpath.of_string path with
  | Error (`Msg s) -> Error [ Err_templates.invalid_path_error s ]
  | Ok path -> (
      match Parser.parse path with
      | Error (`FileError err) ->
          Error [ Err_templates.cannot_read_file_error err ]
      | Ok ast -> ast)

let ( let* ) = Result.( >>= )

(* - API - *)

type compiler_options = { path : string; dump_ast : bool }

let compile { path; dump_ast } =
  let* ast = load_entryfile path in
  if dump_ast then (
    print_endline (Ast.show_ast ast);
    Ok ())
  else
    let* last = Lookup.lookup ast in
    print_endline (Lookup_ast.show_lookup_ast last);
    Ok ()

let run_compiler options =
  match compile options with
  | Ok _ -> ()
  | Error errs -> print_endline (Errs.show_errs errs)
