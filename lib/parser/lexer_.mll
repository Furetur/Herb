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
let string_literal = '"' string_literal_char* '"'

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
| "import" { IMPORT }
| "for" { FOR }
| "extern" { EXTERN }

(* Atoms *)
| id as name
    { ID (name) }
| digit+ as i
    { INT (int_of_string i) }
| '"' (string_literal_char* as s) '"'
    { STRING ( s ) }

(* Operators *)
| "="   { DEF }
| ":="  { ASSIGN }

| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { MUL }
| '/'  { DIV }
| '%'  { MOD }

| "||" { OR }
| "&&" { AND }
| '!'  { NOT }

| '<'  { LT }
| "<=" { LTE }
| "==" { EQ }
| "!=" { NEQ }
| ">=" { GTE }
| ">"  { GT }


(* Punctuation *)
| ','  { COMMA }
| ':'  { COLON }
| ';'  { SEMI }
| "->" { ARROW }
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| ".." { DOTDOT }
| '.'  { DOT }
| eof  { EOF }

| _ { raise Error }

