{
  open Parser

  exception Error of string
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9' '_']*

rule token = parse
(* Whitespace *)
| [' ' '\t' '\n']
    { token lexbuf }

(* Keywords *)
| "let" { LET }
| "fun" { FUN }
| "entry" { ENTRY }

(* Atoms *)
| id as name
    { ID (name) }
| digit+ as i
    { INT (int_of_string i) }

(* Operators *)
| '='  { ASSIGN }

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
| eof  { EOF }

| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

