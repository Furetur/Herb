open Base
open Stdio

let get_loc buf = (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)

let parse file : (Ast.ast Pass.result, [> `FileError of string ]) Result.t =
  let parse chan =
    let buf = Lexing.from_channel chan in
    Lexing.set_filename buf (Fpath.to_string file);
    try Ok (Ok (Parser_.herbfile Lexer_.token buf))
    with Lexer_.Error | Parser_.Error ->
      let loc = get_loc buf in
      let err = Err_templates.syntax_error loc in
      Ok (Error [ err ])
  in
  try In_channel.with_file ~f:parse (Fpath.to_string file)
  with Sys_error err -> Error (`FileError err)
