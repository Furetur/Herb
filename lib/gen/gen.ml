open Ollvm.Ez.Value
open Ollvm.Ez.Instr
open Ollvm.Ez.Block
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer
open Loc

(* ----- State ------ *)

(* ----- Pass ----- *)

let gen_entry m _ =
  let m, main = M.global m T.i32 "main" in
  let m, entry_bb = M.locals m T.label [ "entry" ] in
  match entry_bb with
  | [ entry_bb ] ->
      let main_fn = define main [] [ block entry_bb [ ret (i32 1) ] ] in
      let m = M.definition m main_fn in
      m
  | _ -> assert false

let gen_module ({ loc = _; value = { entry; _ } } : Typed_ast.typed_ast) =
  let m = M.init "name" ("arm64", "apple", "macosx13.0.0") "" in
  let m = gen_entry m entry in
  m
