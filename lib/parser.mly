%token EOF

// Atoms
%token <int> INT
%token <string> ID

// Operators
%token ASSIGN "="
%token PLUS "+" MINUS "-" MUL "*" DIV "/" MOD "%"
%token OR "||" AND "&&" NOT "!"
%token LT "<" LTE "<=" EQ "==" NEQ "!=" GTE ">=" GT ">"

// Punctuation
%token COMMA "," COLON ":" SEMI ";" ARROW "->" LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}"

//Keywords
%token LET "let" FUN "fun" ENTRY "entry"


%{ open Parsetree %}
%start <Parsetree.herbfile> herbfile

%%

(* -------------------------------------------------------------------------- *)

let herbfile :=
  d=toplevel_decl*; EOF; <> 

(* -----    Types    ----- *)


(* ----- Declarations ----- *)

let raw_let_decl := LET; name=ID; "="; e=expr; { (name, e) }

let toplevel_decl :=
  located (
    | ENTRY; e=expr_block; { PEntry e }
    | d=raw_let_decl; ";"; { PToplevelLet d }
  )

(* ----- Expressions ----- *)

let expr_block := LBRACE; exprs=separated_list(";", expr); RBRACE; { exprs }

let expr :=
  | additive_expr
  | located( 
    | exprs=expr_block; { PExprBlock exprs }
    | d=raw_let_decl; { PLet d }
    )

let additive_expr :=
  | multiplicative_expr
  | located(
      l = additive_expr; op = additive_op; r = multiplicative_expr; { PBinOp (l, op, r) }
    )

let additive_op :=
  | "+";  { PPlus }
  | "-"; { PMinus }

let multiplicative_expr :=
  | atomic_expr
  | located(
      l = multiplicative_expr; op = multiplicative_op; r = atomic_expr; { PBinOp (l, op, r) }
    )

let multiplicative_op :=
  | "*"; { PMul }
  | "/";   { PDiv }

let atomic_expr :=
  | "("; ~ = expr; ")"; <>
  | located(
    | ~ = INT; <PInt>
    | ~ = unary_op; ~ = atomic_expr; <PUnOp>
    )

let unary_op :=
  | MINUS; { PNeg }

(* -------------------------------------------------------------------------- *)

(* [located(x)] recognizes the same input fragment as [x] and wraps its
   semantic value of type ['a] as a value of type ['a located]. *)

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
