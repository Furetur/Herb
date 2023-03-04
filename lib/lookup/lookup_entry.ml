open Base
open Loc
open Proj
open Ast

(* ----- Ast Tree ----- *)

type entry = block located [@@deriving show]

type top_decl_raw = LeLet of raw_let_decl | LeExtern of raw_extern
[@@deriving show]

type top_decl = top_decl_raw located [@@deriving show]

type lib_module = {
  cu : cu;
  resolved_imports : resolved_import list;
  decls : top_decl list;
}
[@@deriving show]

type entry_module = { entry : entry; lib_module : lib_module } [@@deriving show]

type ast = { entry_module : entry_module; lib_modules : lib_module list }
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

let visit_top { loc; value } =
  match value with
  | AEntry entry -> set_entry (locate loc ~value:entry)
  | AToplevelLet decl -> add_decl (locate loc ~value:(LeLet decl))
  | AExtern ex -> add_decl (locate loc ~value:(LeExtern ex))

let visit_parsed_file ({ decls; resolved_imports; cu } : ast_module) =
  let* _ = fold_state decls ~f:visit_top in
  let* { entry; decls; _ } = access in
  return (entry, { decls; resolved_imports; cu })

(* ----- Result ----- *)

let visit_lib_module m =
  let* _, lib = visit_parsed_file m in
  return_final lib

let visit_entry_module m =
  let* entry, lib = visit_parsed_file m in
  match entry with
  | Some entry -> return_final { entry; lib_module = lib }
  | None ->
      fail_final
        (Errs.err ~title:"This module must have an entrypoint"
           ~text:"Define an entry point using `entry {}`" m.cu Loc.start_loc)

(* ----- Runners ----- *)

let init = { decls = []; entry = None }
let process_lib_file (f : ast_module) = run_pass f.cu (visit_lib_module f) ~init

let process_entry_file (f : ast_module) =
  run_pass f.cu (visit_entry_module f) ~init

let lookup_entry { Ast.lib_modules = libs; Ast.entry_module = entry } =
  let ( let* ) = Result.( >>= ) in
  let* libs =
    List.fold_result libs ~init:[] ~f:(fun libs f ->
        let* lib = process_lib_file f in
        Ok (lib :: libs))
  in
  let* entry = process_entry_file entry in
  Ok { lib_modules = libs; entry_module = entry }
