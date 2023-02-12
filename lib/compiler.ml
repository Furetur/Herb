open Base
open Stdio

let herbariums = Map.empty (module String)

let show_errs errs =
  let errs = List.map errs ~f:Errs.show_err in
  String.concat ~sep:"\n" errs

let compile path =
  match Fpath.of_string path with
  | Error (`Msg s) -> print_endline ("Invalid path:" ^ s)
  | Ok entry -> (
      let proj = Proj.proj_at_cwd ~entry herbariums in
      match Loader.load_project proj with
      | Loaded x -> print_endline (Printf.sprintf "Loaded %d modules" x)
      | Errors x -> print_endline (show_errs x)
      | EntryFileError (p, s) ->
          print_endline
            (Printf.sprintf "Path not valid: %s %s" s (Fpath.to_string p)))
