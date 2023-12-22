open Base
open Stdio

let ( let* ) = Result.( >>= )

let herbc_path =
  let first_arg = Array.get (Sys.get_argv ()) 0 in
  Fpath.v first_arg

let runtime_obj_path = Fpath.(parent herbc_path / "runtime.o")

let run_clang llpath outpath =
  (* TODO: remove this warning *)
  let cmd =
    "clang -Wno-override-module "
    ^ Fpath.to_string runtime_obj_path
    ^ " " ^ Fpath.to_string llpath ^ " -o " ^ Fpath.to_string outpath
  in
  let exitcode = Stdlib.Sys.command cmd in
  if not (exitcode = 0) then print_endline "Clang returned a non-zero exit code"

let gen ir ll_outpath exe_outpath =
  let m = Ll_module.gen_module ir in
  let* _ = Ll_write.write_to_file m ll_outpath in
  run_clang ll_outpath exe_outpath;
  Ok ()
