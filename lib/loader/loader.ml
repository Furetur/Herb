open Base
open Stdio
open Proj
open Parsetree

type loaded_module = {
  cu : cu;
  imports : (string, cu, String.comparator_witness) Map.t;
  tree : Parsetree.herbfile;
}

type module_tbl = (cu, loaded_module, Cu_comparator.comparator_witness) Map.t
type loader_state = { proj : proj; tbl : module_tbl }

module P = Passes.Pass (struct
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

let add_syntax_err loc = add_err ~title:"Illegal syntax" loc

let show_import import =
  let { Loc.value = { herbarium; path }; _ } = import in
  let path = String.concat ~sep:"." path in
  match herbarium with
  | Some herbarium -> Printf.sprintf "%s:%s" herbarium path
  | None -> path

let show_cu_path cu = Fpath.to_string (cu_path cu)

(* --- Pass --- *)

let import_alias import =
  let { Loc.value = { path; _ }; _ } = import in
  List.last_exn path

let resolve_import import =
  let { Loc.loc; Loc.value = { herbarium; path } } = import in
  let alias = import_alias import in
  let* { proj; _ } = access in
  match herbarium with
  | None -> return (Some (alias, Proj.resolve_rel_import proj ~path))
  | Some herbarium -> (
      match Proj.resolve_abs_import proj ~herbarium ~path with
      | Ok cu -> return (Some (alias, cu))
      | Error `UnknownHerbarium ->
          let msg = Printf.sprintf "Unknown herbarium '%s'" herbarium in
          let* _ = add_err ~title:msg loc in
          return None)

let parse_cu cu =
  let path = Fpath.to_string (cu_path cu) in
  Logs.debug (fun m -> m "Parsing %s" path);
  try In_channel.with_file ~f:Parser.parse path
  with Sys_error err -> Error (`FileError err)

let rec load_import import =
  let* cur_cu = get_cu in
  let* imported_cu = resolve_import import in
  match imported_cu with
  | Some (alias, imported_cu) ->
      Logs.debug (fun m ->
          m "Import resolved: '%s' -> %s (from %s)" (show_import import)
            (show_cu_path imported_cu) (show_cu_path cur_cu));
      load_imported_cu import.Loc.loc imported_cu
      *> return (Some (alias, imported_cu))
  | _ ->
      Logs.debug (fun m ->
          m "Import unresolved '%s' (from %s)" (show_import import)
            (show_cu_path cur_cu));
      return None

and load_imported_cu import_loc cu =
  let* quit = has_errs in
  let* already_loaded = is_already_loaded cu in
  if (not quit) && not already_loaded then
    let* from_cu = get_cu in
    set_cu cu
    *> (match parse_cu cu with
       | Ok tree -> load_parsed_cu cu tree
       | Error (`SyntaxError loc) -> add_syntax_err loc
       | Error (`FileError err) ->
           add_err ~title:"Could not read file" ~text:err import_loc)
    *> set_cu from_cu
  else return ()

and load_parsed_cu cu tree =
  let import_nodes = tree.Parsetree.imports in
  (* Avoid revisiting this module *)
  let empty_tbl = Map.empty (module String) in
  let* _ = put_in_tbl cu { cu; imports = empty_tbl; tree } in
  let* imports = many import_nodes ~f:load_import in
  Logs.debug (fun m -> m "Loaded %s" (show_cu_path cu));
  let* imports =
    match Map.of_alist (module String) (List.filter_opt imports) with
    | `Duplicate_key alias ->
        let { Loc.loc; _ } =
          List.find_exn import_nodes ~f:(fun imp ->
              String.(import_alias imp = alias))
        in
        let* _ =
          add_err
            ~title:
              (Printf.sprintf "Import alias '%s' is declared multiple times"
                 alias)
            loc
        in
        return empty_tbl
    | `Ok map -> return map
  in
  put_in_tbl cu { cu; imports; tree }

let load_entry_cu cu =
  set_cu cu
  *>
  match parse_cu cu with
  | Ok tree -> load_parsed_cu cu tree *> return (Ok ())
  | Error (`SyntaxError loc) -> add_syntax_err loc *> return (Ok ())
  | Error (`FileError err) -> return (Error (`FileError err))

(* --- Result --- *)

type schedule = {
  lib_modules : loaded_module list;
  entry_module : loaded_module;
}

type loading_result =
  | EntryFileError of Fpath.t * string
  | Errors of Errs.err list
  | Loaded of schedule

let load_project proj : loading_result =
  Logs.info (fun m -> m "Loading project");
  Logs.info (fun m -> m "\tentry at %s" (show_cu_path proj.entry));
  Logs.info (fun m -> m "\troot at %s" (Fpath.to_string proj.root));

  let s = { proj; tbl = Map.empty (module Proj.Cu_comparator) } in
  let s, errs, res = run_pass proj.entry (load_entry_cu proj.entry) ~init:s in
  let tbl = s.tbl in
  match (res, errs) with
  | Error (`FileError err), _ -> EntryFileError (cu_path proj.entry, err)
  | Ok (), [] -> (
      let get_imports cu =
        Option.(
          value_or_thunk
            (Map.find tbl cu >>| fun m -> Map.data m.imports)
            ~default:(fun () -> []))
      in
      Logs.debug (fun m -> m "Sorting a graph of %d modules" (Map.length tbl));
      match Tsort.tsort get_imports proj.entry with
      | Ok schedule ->
          Logs.info (fun m ->
              m "Module schedule: %s"
                (String.concat ~sep:", " (List.map schedule ~f:show_cu_path)));
          let libs =
            List.filter schedule ~f:(fun cu ->
                Caml.Bool.not (equal_cu cu proj.entry))
          in
          let libs = List.map libs ~f:(fun cu -> Map.find_exn tbl cu) in
          let entry = Map.find_exn tbl proj.entry in
          let schedule = { lib_modules = libs; entry_module = entry } in
          Loaded schedule
      | Error cycle ->
          Logs.info (fun m -> m "Dependency cycle detected");
          let text =
            String.concat ~sep:"->\n  "
              (List.map cycle ~f:(fun cu -> Fpath.to_string (Proj.cu_path cu)))
          in
          Errors
            [
              Errs.err ~title:"Dependency cycle detected" ~text proj.entry
                Loc.start_loc;
            ])
  | Ok (), errs -> Errors errs
