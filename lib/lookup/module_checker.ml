(* open Base
open Loc
open Ast
open Loader

(* ----- Tree ----- *)

type entry = block located [@@deriving show]

type lib_module = {
  cu : cu;
  resolved_imports : resolved_import list;
  decls : top_decl list;
}
[@@deriving show]

type ast = { entry_module : ast_module; lib_modules : ast_module list }
[@@deriving show]

(* ------ State ------ *)

type state = { decls : top_decl list; entry : entry option }

module P = Passes.Pass (struct
  type t = state
end)

open P

let set_entry entry =
  let* s = access in
  match s.entry with
  | None -> put { s with entry = Some entry }
  | Some _ -> add_err ~title:"Entry already defined" entry.loc

let add_decl decl =
  let* s = access in
  put { s with decls = List.append s.decls [ decl ] }

(* ----- Pass ----- *)

let visit_parsed_top = function
  | PTopEntry entry -> set_entry entry
  | PTopDecl decl -> add_decl decl

let visit_parsed_file ({ decls; _ } : parsed_file) =
  let* _ = fold_state decls ~f:visit_parsed_top in
  let* { entry; decls; _ } = access in
  return (entry, decls)

(* ----- Result ----- *)

let visit_lib_file { cu; imports; parsed_file } =
  let* _, decls = visit_parsed_file parsed_file in
  return_final { cu; resolved_imports = imports; decls }

let visit_entry_file { cu; imports; parsed_file } =
  let* entry, decls = visit_parsed_file parsed_file in
  let lib = { cu; resolved_imports = imports; decls } in
  match entry with
  | Some entry -> return_final { entry; module_ = lib }
  | None ->
      fail_final
        (Errs.err ~title:"This module must have an entrypoint"
           ~text:"Define an entry point using `entry {}`" cu Loc.start_loc)

(* ----- Runners ----- *)

let init = { decls = []; entry = None }
let process_lib_file f = run_pass f.cu (visit_lib_file f) ~init
let process_entry_file f = run_pass f.cu (visit_entry_file f) ~init

let check_modules (libs, entry) =
  let ( let* ) = Result.( >>= ) in
  let* libs =
    List.fold_result libs ~init:[] ~f:(fun libs f ->
        let* lib = process_lib_file f in
        Ok (lib :: libs))
  in
  let* entry = process_entry_file entry in
  Ok { lib_modules = libs; entry_module = entry } *)
