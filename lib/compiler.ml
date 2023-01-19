open Parsetree 


let process chan =
  let linebuf = Lexing.from_channel chan in
  try
    (* Run the parser on this line of input. *)
    let x = Parser.prog Lexer.token linebuf in
    let s = show_prog x in
    print_endline s;
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
