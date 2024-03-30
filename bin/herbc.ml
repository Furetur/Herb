open Cmdliner
open Herb.Compiler_options
open Herb.Compiler

let input_file =
  let doc = "An Herb file" in
  let info = Arg.info [] ~doc in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let result =
  let conv =
    Arg.enum
      [
        ("parsetree", Parsetree);
        ("lookuptree", Lookuptree);
        ("ir", Ir);
        ("bin", Binary);
      ]
  in
  let doc =
    "The expected result of running the compiler. Allowed values are: \
     parsetree, lookuptree, ir, bin"
  in
  let info = Arg.info [ "result" ] ~doc in
  Arg.value (Arg.opt conv Binary info)

let output_file =
  let doc = "A file to which the output binary should be written" in
  let info = Arg.info [ "o"; "output" ] ~doc in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let setup_log level =
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let fatal msg =
  print_endline msg;
  exit 1

let herbc log result input_file output_file =
  setup_log log;
  let options =
    make_compiler_options ~inpath:input_file ~bin_outpath:output_file
      ~expected_compilation_result:result
  in
  match options with
  | Error msg -> fatal msg
  | Ok options -> (
      match compile options with Ok () -> 0 | Error msg -> fatal msg)

let herbc_t =
  Term.(const herbc $ Logs_cli.level () $ result $ input_file $ output_file)

let cmd =
  let doc = "Herb Compiler" in
  let info = Cmd.info "herbc" ~doc in
  Cmd.v info herbc_t

let () = Stdlib.exit (Cmd.eval' cmd)
