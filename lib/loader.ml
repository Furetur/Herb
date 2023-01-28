open Stdio

let parse path =
  let parse_chan c =
    let buf = Lexing.from_channel c in
    try
      let x = Parser.prog Lexer.token buf in
      Ok x
    with
    | Lexer.Error msg -> Error msg
    | Parser.Error ->
        let msg =
          Printf.sprintf "At offset %d: syntax error.\n%!"
            (Lexing.lexeme_start buf)
        in
        Error msg
  in
  In_channel.with_file path ~f:parse_chan

let load_project file = parse file
