open Base
open Stdio

let ( let* ) = Result.( >>= )

(* TODO: this only works when CWD = project root *)
let builtins_c_file_path = Fpath.(v "runtime" / "builtins.c")

let run_clang llpath outpath =
  (* TODO: remove this warning *)
  let cmd =
    "clang -Wno-override-module "
    ^ Fpath.to_string builtins_c_file_path
    ^ " " ^ Fpath.to_string llpath ^ " -o " ^ Fpath.to_string outpath
  in
  let exitcode = Stdlib.Sys.command cmd in
  if not (exitcode = 0) then print_endline "Clang returned a non-zero exit code"

let gen ir ll_outpath exe_outpath =
  let m = Ll_module.gen_module ir in
  let* _ = Ll_write.write_to_file m ll_outpath in
  run_clang ll_outpath exe_outpath;
  Ok ()
