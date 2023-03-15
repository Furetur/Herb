open Base
open Stdio
open Loc

let get_loc buf = (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)

let parse ~errpath file :
    (Ast.parsed_file, [> `FileError of string | `SyntaxError of loc ]) Result.t
    =
  let parse chan =
    let buf = Lexing.from_channel chan in
    Lexing.set_filename buf (Fpath.to_string errpath);
    try Ok (Parser_.herbfile Lexer_.token buf)
    with Lexer_.Error | Parser_.Error ->
      let loc = get_loc buf in
      Error (`SyntaxError loc)
  in
  try In_channel.with_file ~f:parse (Fpath.to_string file)
  with Sys_error err -> Error (`FileError err)
