open Base
open Stdio

let herbariums = Map.empty (module String)

let compile_loaded schedule =
  match Lowering.lower schedule with
  | Ok ast ->
      print_endline (Ast.show_ast ast);
      print_endline "Loaded and lowered"
  | Error errs -> print_endline (Errs.show_errs errs)

let compile path =
  match Fpath.of_string path with
  | Error (`Msg s) -> print_endline ("Invalid path:" ^ s)
  | Ok entry -> (
      let proj = Proj.proj_at_cwd ~entry herbariums in
      match Loader.load_project proj with
      | Loaded schedule -> compile_loaded schedule
      | Errors x -> print_endline (Errs.show_errs x)
      | EntryFileError (_, s) ->
          print_endline
            Errs.(
              show_simple_err ImportError ~title:"Could not open file" ~text:s)
      | DependencyCycle cycle ->
          let title = "Dependency cycle detected" in
          let cycle =
            String.concat ~sep:"->\n  "
              (List.map cycle ~f:(fun cu -> Fpath.to_string (Proj.cu_path cu)))
          in

          print_endline Errs.(show_simple_err ImportError ~title ~text:cycle))
