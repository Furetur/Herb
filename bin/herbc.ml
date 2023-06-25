open Cmdliner
open Herb.Compiler

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
  let doc = "An entryfile to the project" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let setup_log level =
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let herbc log dump_parsetree dump_lookuptree dump_typedtree input_file =
  setup_log log;
  run_compiler
    { path = input_file; dump_parsetree; dump_lookuptree; dump_typedtree }

let herbc_t =
  Term.(
    const herbc $ Logs_cli.level () $ dump_parsetree $ dump_lookuptree
    $ dump_typedtree $ input_file)

let cmd =
  let doc = "Herb Compiler" in
  let info = Cmd.info "herb" ~doc in
  Cmd.v info herbc_t

let () = exit (Cmd.eval cmd)
