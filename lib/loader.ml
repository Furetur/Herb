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

let fail err = return (Error [ err ])
let return_ok x = return (Ok x)

let rec fold_errs xs f =
  match xs with
  | [] -> return_ok ()
  | x :: xs -> (
      let* r = f x in
      match r with Ok _ -> fold_errs xs f | Error err -> return (Error err))

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
  try In_channel.with_file ~f:Parser.parse path
  with Sys_error err -> Error (`FileError err)

let rec load_import from_cu import =
  let { Loc.loc; _ } = import in
  let* imported_cu = resolve_import from_cu import in
  match imported_cu with
  | Some imported_cu ->
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
  | _ -> return None

and load_parsed_cu cu tree =
  let imports = tree.Parsetree.imports in
  (* Avoid revisiting this module *)
  let* _ = put_in_tbl cu { cu; imports = []; tree } in
  let* imports = many imports ~f:(load_import cu) in
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
  let s = { proj; errs = []; tbl = Map.empty (module Proj.Cu_comparator) } in
  let s, res = run_state (load_entry_cu proj.entry) ~init:s in
  match (res, s.errs) with
  | Error (`FileError err), _ -> EntryFileError (cu_path proj.entry, err)
  | Ok (), [] -> Loaded (Map.length s.tbl)
  | Ok (), errs -> Errors errs
