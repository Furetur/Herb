open Base
open Stdio

let get_loc buf = (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)

let parse file : (Ast.ast Pass.result, [> `FileError of string ]) Result.t =
  let aux_parse chan =
    let buf = Lexing.from_channel chan in
    Lexing.set_filename buf (Fpath.to_string file);
    try
      let raw_ast = Parser_.herbfile Lexer_.token buf in
      let loc = Loc.infile_loc file in
      Ok (Ok { Loc.loc; value = raw_ast })
    with Lexer_.Error | Parser_.Error ->
      let loc = get_loc buf in
      let err = Err_templates.syntax_error loc in
      Ok (Error [ err ])
  in
  try In_channel.with_file ~f:aux_parse (Fpath.to_string file)
  with Sys_error err -> Error (`FileError err)
