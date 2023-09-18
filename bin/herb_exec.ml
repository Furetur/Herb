open Base
open Cmdliner
open Herb
open Herb.Compiler

let ( let* ) = Result.( >>= )

let input_file =
  let doc = "An Herb file" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let run_binary_file path = Stdlib.Sys.command (Fpath.to_string path)

let exec input_file =
  let result =
    let* outpath =
      compile
        {
          path = input_file;
          outpath = None;
          dump_parsetree = false;
          dump_lookuptree = false;
          dump_typedtree = false;
        }
    in
    let exitcode = run_binary_file outpath in
    Ok exitcode
  in
  Errs.handle_comp_result result

let exec_t = Term.(const exec $ input_file)

let cmd =
  let doc = "Execute Herb Files" in
  let info = Cmd.info "herb_exec" ~doc in
  Cmd.v info exec_t

let () = Stdlib.exit (Cmd.eval' cmd)
