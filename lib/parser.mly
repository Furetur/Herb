%token EOF

// Atoms
%token <int> INT
%token <string> ID

// Operators
%token ASSIGN "="
%token PLUS "+" MINUS "-" MUL "*" DIV "/" MOD "%"
%token  OR "||" AND "&&" NOT "!"
%token LT "<" LTE "<=" EQ "==" NEQ "!=" GTE ">=" GT ">"

// Punctuation
%token COMMA "," COLON ":" SEMI ";" ARROW "->" LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}"

//Keywords
%token LET "let" FUN "fun" ENTRY "entry"


%{ open Parsetree %}
%start <Parsetree.prog> prog

%%

(* -------------------------------------------------------------------------- *)

let prog :=
  d=decl*; EOF; <> 

(* ----- Declarations ----- *)

let decl :=
  located ( ENTRY; "{"; e = expr; "}"; { PEntry e } )

(* ----- Expressions ----- *)

let expr :=
  additive_expr

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
