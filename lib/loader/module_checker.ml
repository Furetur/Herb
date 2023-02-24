open Base
open Ast
open Fs_loader

type module_kind = Entry | Lib
type state = { kind : module_kind; decls : top_decl list; entry : entry option }

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
  fold_state decls ~f:visit_parsed_top

(* ----- Result ----- *)

let process_file { cu; imports; parsed_file } =
  let s, errs, _ =
    run_pass cu
      (visit_parsed_file parsed_file)
      ~init:{ kind = Lib; decls = []; entry = None }
  in
  match errs with
  | [] -> Ok (s.entry, { cu; resolved_imports = imports; decls = s.decls })
  | errs -> Error (`Errs errs)

let process_lib_file f =
  match process_file f with
  | Ok (_, m) -> Ok m
  | Error (`Errs errs) -> Error (`Errs errs)

let process_entry_file cu f =
  match process_file f with
  | Ok (None, _) ->
      Error
        (`Errs
          [
            Errs.err ~title:"This module must have an entrypoint"
              ~text:"Define an entry point using `entry {}`" cu Loc.start_loc;
          ])
  | Ok (Some entry, m) -> Ok { entry; module_ = m }
  | Error (`Errs errs) -> Error (`Errs errs)
