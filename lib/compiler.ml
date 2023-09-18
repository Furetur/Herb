open Base
open Stdio
module P = Ollvm.Printer
module M = Ollvm.Ez.Module

(* - Helpers - *)

let ( let* ) = Result.( >>= )

let get_paths inpath outpath =
  let* inpath = Util.parse_path inpath in
  let llpath = Fpath.set_ext ".ll" inpath in
  let* outpath =
    match outpath with
    | Some path -> Util.parse_path path
    | None -> Ok (Fpath.set_ext ".exe" inpath)
  in
  Ok (inpath, llpath, outpath)

let load_entryfile path : Ast.ast Pass.result =
  match Parser.parse path with
  | Error (`FileError err) -> Error [ Err_templates.cannot_read_file_error err ]
  | Ok ast -> ast

let write_ll (m : M.t) llpath =
  Out_channel.with_file (Fpath.to_string llpath) ~append:false ~f:(fun out ->
      let formatter = Stdlib.Format.formatter_of_out_channel out in
      P.modul (P.empty_env ()) formatter m.m_module)

let run_clang llpath outpath =
  let cmd =
    "clang " ^ Fpath.to_string llpath ^ " -o " ^ Fpath.to_string outpath
  in
  let exitcode = Stdlib.Sys.command cmd in
  if not (exitcode = 0) then failwith "Clang returned a non-zero exit code"

(* - API - *)

type compiler_options = {
  path : string;
  outpath : string option;
  dump_parsetree : bool;
  dump_lookuptree : bool;
  dump_typedtree : bool;
}

let compile { path; outpath; dump_parsetree; dump_lookuptree; dump_typedtree } =
  let* inpath, llpath, outpath = get_paths path outpath in
  (* Compile *)
  let* ast = load_entryfile inpath in
  if dump_parsetree then print_endline (Ast.show_ast ast);
  let* last = Lookup.lookup ast in
  if dump_lookuptree then print_endline (Lookup_ast.show_lookup_ast last);
  let* tast = Typing.check last in
  if dump_typedtree then print_endline (Typed_ast.show_typed_ast tast);
  let m = Gen.gen_module tast in
  write_ll m llpath;
  run_clang llpath outpath;
  Ok outpath
