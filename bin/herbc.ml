open Cmdliner

let dump_parsetree =
  let doc = "Dump parse tree" in
  let info = Arg.info [ "pt"; "dump-parsetree" ] ~doc in
  Arg.value (Arg.flag info)

let input_file =
  let doc = "An entryfile to the project" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let setup_log level =
  Fmt_tty.setup_std_outputs ~style_renderer:(`Ansi_tty) ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let herbc log _ input_file = 
  setup_log log;
  Herb.Compiler.compile input_file
let herbc_t = Term.(const herbc $ Logs_cli.level () $ dump_parsetree $ input_file)

let cmd =
  let doc = "Herb Compiler" in
  let info = Cmd.info "herb" ~doc in
  Cmd.v info (herbc_t)

let () = exit (Cmd.eval cmd)
