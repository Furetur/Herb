open Cmdliner
open Herb.Compiler

let input_file =
  let doc = "An Herb file" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let exec input_file =
  let outpath =
    run_compiler
      {
        path = input_file;
        outpath = None;
        dump_parsetree = false;
        dump_lookuptree = false;
        dump_typedtree = false;
      }
  in
  match outpath with
  | Some outpath ->
      let exitcode = Stdlib.Sys.command (Fpath.to_string outpath) in
      Stdlib.exit exitcode
  | None -> Stdlib.exit 1

let exec_t = Term.(const exec $ input_file)

let cmd =
  let doc = "Execute Herb Files" in
  let info = Cmd.info "herb_exec" ~doc in
  Cmd.v info exec_t

let () = Stdlib.exit (Cmd.eval cmd)
