open Lexing
open Base
open Stdio

let get_loc buf = (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)
let line_col pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
let start_line_col (pos, _) = line_col pos

let raise_error errtype loc =
  let line, col = start_line_col loc in
  Printf.failwithf "%s: line %d, col %d" errtype line col ()

let parse (filepath : Fpath.t) : Parsetree.parsetree =
  let aux_parse chan =
    let buf = Lexing.from_channel chan in
    Lexing.set_filename buf (Fpath.to_string filepath);
    try Parser_.herbfile Lexer_.token buf with
    | Lexer_.Error -> raise_error "Lexing error" (get_loc buf)
    | Parser_.Error -> raise_error "Parsing error" (get_loc buf)
  in
  try In_channel.with_file ~f:aux_parse (Fpath.to_string filepath)
  with Sys_error err -> failwith err
