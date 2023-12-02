{
  open Lexing
  open Parser_

  exception Error

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
        }
}

let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9' '_']*

let string_literal_char = ("\\" _) | [^ '"' '\\']

rule token = parse
(* Whitespace *)
| whitespace { token lexbuf }
| newline { next_line lexbuf; token lexbuf}

(* Keywords *)
| "let" { LET }
| "entry" { ENTRY }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "return" { RETURN }

(* Atoms *)
| id as name
    { ID (name) }
| ('-'? digit+) as i
    { INT (int_of_string i) }

(* Operators *)
| "="   { DEF }

| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { MUL }
| '/'  { DIV }
| '%'  { MOD }

| '<' { LT }
| "<=" { LTE }
| "==" { EQ }
| "!=" { NEQ }
| ">" { GT }
| ">=" { GTE }

(* Punctuation *)
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| ';'  { SEMI }
| ","  { COMMA }
| eof  { EOF }

| _ { raise Error }

