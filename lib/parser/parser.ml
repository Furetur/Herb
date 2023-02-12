open Base
open Stdio

let with_file path f =
  try In_channel.with_file ~f:(fun x -> f (Ok x)) (Fpath.to_string path)
  with Sys_error err -> f (Error err)

let get_loc buf = (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)

let parse chan =
  let buf = Lexing.from_channel chan in
  try
    let x = Parser_.herbfile Lexer_.token buf in
    Ok x
  with
  | Lexer_.Error -> Error (`SyntaxError (get_loc buf))
  | Parser_.Error -> Error (`SyntaxError (get_loc buf))
