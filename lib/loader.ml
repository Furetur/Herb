open Base
open Stdio
open Lexing

let linecol pos =
  (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let parse path =
  let parse_chan c =
    let buf = Lexing.from_channel c in
    try
      let x = Parser.herbfile Lexer.token buf in
      Ok x
    with
    | Lexer.Error msg -> Error msg
    | Parser.Error ->
        let sline, scol = linecol (Lexing.lexeme_start_p buf) in
        let eline, ecol = linecol (Lexing.lexeme_end_p buf) in
        let msg =
          Printf.sprintf "%s: from %d:%d to %d:%d: syntax error.\n%!" path sline scol eline ecol
        in
        Error msg
  in
  In_channel.with_file path ~f:parse_chan


let load_project file = parse file
