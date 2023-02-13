open Base
open Stdio
open Proj
open Errs
open Parsetree

type loaded_module = { cu : cu; imports : cu list; tree : Parsetree.herbfile }
type module_tbl = (cu, loaded_module, Cu_comparator.comparator_witness) Map.t
type loader_state = { proj : proj; tbl : module_tbl; errs : err list }

module P = Monads.StateMonad (struct
  type t = loader_state
end)

open P

let put_in_tbl mod_desc m =
  let* s = access in
  let tbl = Map.set s.tbl ~key:mod_desc ~data:m in
  put { s with tbl }

let is_already_loaded m =
  let* { tbl; _ } = access in
  return (Map.mem tbl m)

let has_errors =
  let* { errs; _ } = access in
  return (List.length errs <> 0)

let add_err err =
  let* s = access in
  put { s with errs = err :: s.errs }

let add_syntax_err cu loc =
  add_err { cu; loc; kind = SyntaxError; title = "Illegal syntax"; text = "" }

let show_import import =
  let { Loc.value = { herbarium; path }; _ } = import in
  let path = String.concat ~sep:"." path in
  match herbarium with
  | Some herbarium -> Printf.sprintf "%s:%s" herbarium path
  | None -> path

let show_cu_path cu = Fpath.to_string (cu_path cu)

let resolve_import cu import =
  let { Loc.loc; Loc.value = { herbarium; path } } = import in
  let* { proj; _ } = access in
  match herbarium with
  | None -> return (Some (Proj.resolve_rel_import proj ~path))
  | Some herbarium -> (
      match Proj.resolve_abs_import proj ~herbarium ~path with
      | Ok cu -> return (Some cu)
      | Error `UnknownHerbarium ->
          let msg = Printf.sprintf "Unknown herbarium '%s'" herbarium in
          let* _ =
            add_err { cu; loc; kind = ImportError; title = msg; text = "" }
          in
          return None)

let parse_cu cu =
  let path = Fpath.to_string (cu_path cu) in
  Logs.debug (fun m -> m "Parsing %s" path);
  try In_channel.with_file ~f:Parser.parse path
  with Sys_error err -> Error (`FileError err)

let rec load_import from_cu import =
  let { Loc.loc; _ } = import in
  let* imported_cu = resolve_import from_cu import in
  match imported_cu with
  | Some imported_cu ->
      Logs.debug (fun m ->
          m "Import resolved: '%s' -> %s (from %s)" (show_import import)
            (show_cu_path imported_cu) (show_cu_path from_cu));
      let not = Caml.Bool.not in
      let* quit = has_errors in
      let* already_loaded = is_already_loaded imported_cu in
      let* _ =
        if (not quit) && not already_loaded then
          match parse_cu imported_cu with
          | Ok tree -> load_parsed_cu imported_cu tree
          | Error (`SyntaxError loc) -> add_syntax_err imported_cu loc
          | Error (`FileError err) ->
              add_err
                {
                  cu = from_cu;
                  loc;
                  kind = ImportError;
                  title = "Could not read file";
                  text = err;
                }
        else return ()
      in
      return (Some imported_cu)
  | _ ->
      Logs.debug (fun m ->
          m "Import unresolved '%s' (from %s)" (show_import import)
            (show_cu_path from_cu));
      return None

and load_parsed_cu cu tree =
  let imports = tree.Parsetree.imports in
  (* Avoid revisiting this module *)
  let* _ = put_in_tbl cu { cu; imports = []; tree } in
  let* imports = many imports ~f:(load_import cu) in
  Logs.debug (fun m -> m "Loaded %s" (show_cu_path cu));
  put_in_tbl cu { cu; imports = List.filter_opt imports; tree }

let load_entry_cu cu =
  match parse_cu cu with
  | Ok tree -> load_parsed_cu cu tree *> return (Ok ())
  | Error (`SyntaxError loc) -> add_syntax_err cu loc *> return (Ok ())
  | Error (`FileError err) -> return (Error (`FileError err))

type loading_result =
  | EntryFileError of Fpath.t * string
  | Errors of Errs.err list
  | Loaded of int

let load_project proj =
  Logs.info (fun m -> m "Loading project");
  Logs.info (fun m -> m "\tentry at %s" (show_cu_path proj.entry));
  Logs.info (fun m -> m "\troot at %s" (Fpath.to_string proj.root));

  let s = { proj; errs = []; tbl = Map.empty (module Proj.Cu_comparator) } in
  let s, res = run_state (load_entry_cu proj.entry) ~init:s in
  match (res, s.errs) with
  | Error (`FileError err), _ -> EntryFileError (cu_path proj.entry, err)
  | Ok (), [] -> Loaded (Map.length s.tbl)
  | Ok (), errs -> Errors errs
