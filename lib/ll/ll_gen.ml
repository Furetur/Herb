open Base
open Stdio
open Errors
open Pass_dsl.Result_dsl_syntax

let run_clang llpath outpath =
  let env = Ll_env.make_ll_env () in
  (* TODO: remove this warning *)
  let cmd =
    "clang -Wno-override-module "
    ^ Fpath.to_string env.runtime_obj_path
    ^ " " ^ Fpath.to_string llpath ^ " -o " ^ Fpath.to_string outpath
  in
  let exitcode = Stdlib.Sys.command cmd in
  if not (exitcode = 0) then print_endline "Clang returned a non-zero exit code"

let gen ll_outpath exe_outpath ir : unit compilation_result =
  let m = Ll_module.gen_module ir in
  let* _ = Ll_write.write_to_file m ll_outpath in
  run_clang ll_outpath exe_outpath;
  return ()
