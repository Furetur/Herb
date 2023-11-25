open Cmdliner
open Herb.Compiler

let only_parsetree =
  let doc = "Only Parsetree" in
  let info = Arg.info [ "only-parsetree"; "parse" ] ~doc in
  Arg.value (Arg.flag info)

let input_file =
  let doc = "An Herb file" in
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

let herbc log only_parsetree input_file output_file =
  setup_log log;
  let _ =
    compile { path = input_file; outpath = output_file; only_parsetree; only_ir = false }
  in
  0

let herbc_t =
  Term.(
    const herbc $ Logs_cli.level () $ only_parsetree $ input_file $ output_file)

let cmd =
  let doc = "Herb Compiler" in
  let info = Cmd.info "herbc" ~doc in
  Cmd.v info herbc_t

let () = Stdlib.exit (Cmd.eval' cmd)
