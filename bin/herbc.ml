open Base
open Cmdliner
open Herb
open Herb.Compiler

let ( let* ) = Result.( >>= )

let dump_parsetree =
  let doc = "Dump Parsetree" in
  let info = Arg.info [ "dump-parsetree"; "p" ] ~doc in
  Arg.value (Arg.flag info)

let dump_lookuptree =
  let doc = "Dump Lookuptree" in
  let info = Arg.info [ "dump-lookuptree"; "l" ] ~doc in
  Arg.value (Arg.flag info)

let dump_typedtree =
  let doc = "Dump Typedtree" in
  let info = Arg.info [ "dump-typedtree"; "t" ] ~doc in
  Arg.value (Arg.flag info)

let input_file =
  let doc = "A Herb file" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let output_file =
  let doc = "A file to which the output should be written" in
  let info = Arg.info [ "o"; "output" ] ~doc in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let setup_log level =
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let herbc log dump_parsetree dump_lookuptree dump_typedtree input_file
    output_file =
  setup_log log;
  let result =
    let* _ =
      compile
        {
          path = input_file;
          outpath = output_file;
          dump_parsetree;
          dump_lookuptree;
          dump_typedtree;
        }
    in
    Ok ()
  in
  Errs.handle_comp_result_unit result

let herbc_t =
  Term.(
    const herbc $ Logs_cli.level () $ dump_parsetree $ dump_lookuptree
    $ dump_typedtree $ input_file $ output_file)

let cmd =
  let doc = "Herb Compiler" in
  let info = Cmd.info "herbc" ~doc in
  Cmd.v info herbc_t

let () = Stdlib.exit (Cmd.eval' cmd)
