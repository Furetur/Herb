open Base
open Stdio
open Proj
open Ast

type module_tbl = (cu, ast_module, Cu_comparator.comparator_witness) Map.t

type loader_state = {
  proj : proj;
  tbl : module_tbl;
  visited : (cu, Cu_comparator.comparator_witness) Set.t;
}

module P = Passes.Pass (struct
  type t = loader_state
end)

open P

let put_in_tbl mod_desc m =
  let* s = access in
  let tbl = Map.set s.tbl ~key:mod_desc ~data:m in
  put { s with tbl }

let mark_as_loaded cu =
  let* s = access in
  put { s with visited = Set.add s.visited cu }

let is_already_loaded cu =
  let* { visited; _ } = access in
  return (Set.mem visited cu)

let add_syntax_err loc = add_err ~title:"Illegal syntax" loc

let show_import import =
  let { Loc.value = { herbarium; path }; _ } = import in
  let path = String.concat ~sep:"." path in
  match herbarium with
  | Some herbarium -> Printf.sprintf "%s:%s" herbarium path
  | None -> path

(* --- Pass --- *)

let import_alias import =
  let { Loc.value = { path; _ }; _ } = import in
  List.last_exn path

let resolve_import import =
  let { Loc.loc; Loc.value = { herbarium; path } } = import in
  let* { proj; _ } = access in
  match herbarium with
  | None ->
      return (Some { import; imported_cu = Proj.resolve_rel_import proj ~path })
  | Some herbarium -> (
      match Proj.resolve_abs_import proj ~herbarium ~path with
      | Ok imported_cu -> return (Some { import; imported_cu })
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
  let* resolved_import = resolve_import import in
  match resolved_import with
  | Some resolved_import ->
      let { imported_cu; _ } = resolved_import in
      Logs.debug (fun m ->
          m "Import resolved: '%s' -> %s (from %s)" (show_import import)
            (show_cu imported_cu) (show_cu cur_cu));
      load_imported_cu import.Loc.loc imported_cu
      *> return (Some resolved_import)
  | _ ->
      Logs.debug (fun m ->
          m "Import unresolved '%s' (from %s)" (show_import import)
            (show_cu cur_cu));
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

and load_parsed_cu cu file =
  let { imports; decls } = file in
  (* Avoid revisiting this module *)
  mark_as_loaded cu
  *> let* resolved_imports = many imports ~f:load_import in
     let resolved_imports = List.filter_opt resolved_imports in
     Logs.debug (fun m -> m "Loaded %s" (show_cu cu));
     put_in_tbl cu { cu; resolved_imports; decls }

let log_schedule { lib_modules = libs; entry_module = entry } =
  let nlibs = List.length libs in
  Logs.info (fun m ->
      m "Loaded %d modules (%d libs), schedule:" (nlibs + 1) nlibs);
  List.iteri libs ~f:(fun i lib ->
      Logs.info (fun m -> m "\t%d: %s" i (show_cu lib.cu)));
  Logs.info (fun m -> m "\t%d: (ENTRY) %s" nlibs (show_cu entry.cu));
  ()

let tsort_tbl (tbl : module_tbl) entry_cu =
  let get_imported_modules m =
    let cus = List.map m.resolved_imports ~f:(fun i -> i.imported_cu) in
    List.filter_map cus ~f:(fun cu -> Map.find tbl cu)
  in
  let modules = Map.data tbl in
  match Tsort.tsort (module Module_comparator) modules get_imported_modules with
  | Ok schedule ->
      let ( != ) x y = Caml.Bool.not (equal_ast_module x y) in
      let entry = Map.find_exn tbl entry_cu in
      let libs = List.filter schedule ~f:(fun m -> m != entry) in
      let ast = { entry_module = entry; lib_modules = libs } in
      log_schedule ast;
      return_final ast
  | Error (`Cycle cycle) ->
      Logs.info (fun m -> m "Dependency cycle detected");
      let cycle = List.map cycle ~f:(fun m -> m.cu) in
      fail_final (Errs.Templates.dependency_cycle_error cycle)

let load_proj_from_entry cu chan =
  set_cu cu
  *>
  match Parser.parse chan with
  | Ok tree ->
      let* _ = load_parsed_cu cu tree in
      let* { tbl; _ } = access in
      (* TODO: do not run tsort if there are errors (some files failed to load) *)
      tsort_tbl tbl cu
  | Error (`SyntaxError loc) ->
      fail_final (Errs.Templates.syntax_error loc)

(* --- Runners --- *)

let init proj =
  {
    proj;
    tbl = Map.empty (module Cu_comparator);
    visited = Set.empty (module Cu_comparator);
  }

let load_project proj : (ast, _) Result.t =
  let load_project_entry proj chan =
    let res =
      run_pass proj.Proj.entry
        (load_proj_from_entry proj.entry chan)
        ~init:(init proj)
    in
    match res with Ok x -> Ok x | Error x -> Error (`Errs x)
  in

  Logs.info (fun m -> m "Loading project");
  Logs.info (fun m -> m "\tentry at %s" (show_cu proj.Proj.entry));
  Logs.info (fun m -> m "\troot at %s" (Fpath.to_string proj.root));

  let entry_path = cu_path proj.entry in
  try
    In_channel.with_file ~f:(load_project_entry proj)
      (Fpath.to_string entry_path)
  with Sys_error err -> Error (`EntryFileError (entry_path, err))
