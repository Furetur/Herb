open Base
open Loc
open Proj
open Ast

type mark = Loading | Loaded of ast_module
type module_tbl = (cu, mark, Cu_comparator.comparator_witness) Map.t
type loader_state = { proj : proj; tbl : module_tbl }

module P = Pass.Pass (struct
  type t = loader_state
end)

open P

(* ----- Helpers ----- *)

let show_import import =
  let { Loc.value = { herbarium; path }; _ } = import in
  let path = String.concat ~sep:"." path in
  match herbarium with
  | Some herbarium -> Printf.sprintf "%s:%s" herbarium path
  | None -> path

(* ----- State ----- *)

let put_in_tbl cu mark =
  let* s = access in
  let tbl = Map.set s.tbl ~key:cu ~data:mark in
  put { s with tbl }

let mark_as_loading cu = put_in_tbl cu Loading
let save_ast cu ast = put_in_tbl cu (Loaded ast)

let should_load cu =
  let* { tbl; _ } = access in
  match Map.find tbl cu with None -> return true | _ -> return false

let get_proj =
  let* { proj; _ } = access in
  return proj

let get_loaded_tbl =
  let* { tbl; _ } = access in
  let tbl =
    Map.mapi tbl ~f:(fun ~key ~data ->
        match data with
        | Loaded x -> x
        | _ ->
            failwith (Printf.sprintf "CU %s has not been loaded" (show_cu key)))
  in
  return tbl

(* --- Pass --- *)

(* - Imports - *)

let resolve_import import =
  let { Loc.loc; Loc.value = { herbarium; path } } = import in
  let* proj = get_proj in
  match herbarium with
  | None -> return { import; imported_cu = Proj.resolve_rel_import proj ~path }
  | Some herbarium -> (
      match Proj.resolve_abs_import proj ~herbarium ~path with
      | Ok imported_cu -> return { import; imported_cu }
      | Error `UnknownHerbarium -> fail (Errs.unknown_herbarium herbarium loc))

let rec load_import import =
  let* resolved_import = resolve_import import in
  let { imported_cu; _ } = resolved_import in
  Logs.debug (fun m ->
      m "Import resolved: '%s' -> %s" (show_import import) (show_cu imported_cu));
  let* _ = load_imported_cu resolved_import in
  return resolved_import

and parse_imported_cu import_loc cu =
  let path = cu_path cu in
  match Parser.parse ~errpath:path path with
  | Error (`FileError err) ->
      fail (Errs.could_not_read_file_error err import_loc)
  | Error (`SyntaxError loc) -> fail (Errs.syntax_error loc)
  | Ok res -> return res

and load_imported_cu { imported_cu; import = { loc; _ } } =
  let* should_load = should_load imported_cu in
  if should_load then
    let* file = parse_imported_cu loc imported_cu in
    load_parsed_cu imported_cu file
  else return ()

(* - General - *)

and load_parsed_cu cu file =
  let { imports; decls } = file in
  (* Avoid revisiting this module *)
  let* _ = mark_as_loading cu in
  let* resolved_imports = many imports ~f:load_import in
  Logs.debug (fun m -> m "Loaded %s" (show_cu cu));
  save_ast cu { cu; resolved_imports; decls }

(* - Entry - *)
let parse_entry_cu cu =
  let path = cu_path cu in
  match Parser.parse ~errpath:path path with
  | Ok file -> return file
  | Error (`SyntaxError loc) -> fail (Errs.syntax_error loc)
  | Error (`FileError msg) -> fail (Errs.could_not_open_entryfile path msg)

let load_entry =
  let* { entry; _ } = get_proj in
  let* tree = parse_entry_cu entry in
  load_parsed_cu entry tree

(* - Tsort - *)

let log_schedule { lib_modules = libs; entry_module = entry } =
  let nlibs = List.length libs in
  Logs.info (fun m ->
      m "Loaded %d modules (%d libs), schedule:" (nlibs + 1) nlibs);
  List.iteri libs ~f:(fun i lib ->
      Logs.info (fun m -> m "\t%d: %s" i (show_cu lib.cu)));
  Logs.info (fun m -> m "\t%d: (ENTRY) %s" nlibs (show_cu entry.cu));
  ()

let tsort_tbl =
  let* tbl = get_loaded_tbl in
  let* { entry; _ } = get_proj in
  let get_imported_modules m =
    let cus = List.map m.resolved_imports ~f:(fun i -> i.imported_cu) in
    List.filter_map cus ~f:(fun cu -> Map.find tbl cu)
  in
  let modules = Map.data tbl in
  match Tsort.tsort (module Module_comparator) modules get_imported_modules with
  | Ok schedule ->
      let ( != ) x y = Caml.Bool.not (equal_ast_module x y) in
      let entry = Map.find_exn tbl entry in
      let libs = List.filter schedule ~f:(fun m -> m != entry) in
      let ast = { entry_module = entry; lib_modules = libs } in
      log_schedule ast;
      return ast
  | Error (`Cycle cycle) ->
      Logs.info (fun m -> m "Dependency cycle detected");
      let cycle = List.map cycle ~f:(fun m -> m.cu) in
      fail (Errs.dependency_cycle_error cycle)

(* - Project - *)

let load_proj = load_entry *> tsort_tbl

(* - Runners - *)

let init proj = { proj; tbl = Map.empty (module Cu_comparator) }

let load_project proj : (ast, _) Result.t =
  Logs.info (fun m -> m "Loading project");
  Logs.info (fun m -> m "\tentry at %s" (show_cu proj.Proj.entry));
  Logs.info (fun m -> m "\troot at %s" (Fpath.to_string proj.root));

  run_pass load_proj ~init:(init proj)
