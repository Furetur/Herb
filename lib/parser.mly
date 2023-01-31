%token EOF

// Atoms
%token <int> INT
%token <string> ID


// Operators
%token DEF "="
%token ASSIGN ":="
%token PLUS "+" MINUS "-" MUL "*" DIV "/" MOD "%"
%token OR "||" AND "&&" NOT "!"
%token LT "<" LTE "<=" EQ "==" NEQ "!=" GTE ">=" GT ">"

// Punctuation
%token COMMA "," COLON ":" SEMI ";" ARROW "->" LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}"

// Keywords
%token LET ENTRY IF ELSE WHILE


%{ 
  open Parsetree 
  open Loc
%}

%start <Parsetree.herbfile> herbfile

%type <Parsetree.typ> typ
%type <Parsetree.expr> operand primary_expr expr binop_expr unop_expr
%type <Parsetree.expr> binop_assign_expr


%%

(* -------------------------------------------------------------------------- *)

let herbfile :=
  d=toplevel_decl*; EOF; <> 

(* -----    Types    ----- *)

let named_typ := name=ID; { PTypNamed name }

let typ := located(
  t=named_typ; { t }
) 

(* ----- Declarations ----- *)

let raw_let_decl := LET; name=ID; "="; e=expr; { (name, e) }

let toplevel_decl :=
  located (
    | ENTRY; e=expr_block; { PEntry e }
    | d=raw_let_decl; ";"; { PToplevelLet d }
  )

(* ----- Expressions ----- *)

(* -- Operand -- *)

let formal_arg := name=ID; ":"; t=typ; { (name, t) }
let formal_args := args=separated_list(",", formal_arg); { args}

let fun_literal := 
  "("; formal_args=formal_args; ")"; "->"; body=expr; { PFunLiteral { formal_args; body } }

let operand := 
  | located(
      | i = INT; { PInt i }
      | name=ID; { PIdent name }
      | f=fun_literal; { f }
    )
  | "("; e=expr; ")"; { e }

(* -- Primary Expression -- *)

let arguments := exprs=separated_list(",", expr); { exprs }

let primary_expr := 
  | x=operand; { x }
  | located( 
    callee=primary_expr; "("; args=arguments; ")"; { PFunCall {callee; args } } 
  )
  
(* -- Expression -- *)

let expr := 
  | e=binop_expr; { e }
  | located(   
    | exprs=expr_block; { PExprBlock exprs }
    | d=raw_let_decl; { PLet d }
    | IF; cond=expr; then_=expr_block; { PIf {cond; then_; else_=[]} }
    | IF; cond=expr; then_=expr_block; ELSE; else_=expr_block; { PIf {cond; then_; else_} }
    | WHILE; cond=expr; body=expr_block; { PWhile { cond; body } }
  )

let expr_block := LBRACE; exprs=separated_list(";", expr); RBRACE; { exprs }

(* -- Operators -- *)

let unop_expr := 
  | e=primary_expr; { e }
  | located( 
    op=unary_op; e=unop_expr; { PUnOp (op, e) } 
  )

(* From lower precedence to higher *)

let binop_expr := binop_assign_expr

let binop_assign_expr := 
  | located ( 
      l=binop_or_expr; ":="; r=binop_assign_expr; { PAssign (l, r) } 
    )
  | e=binop_or_expr; { e }

let binop_or_expr := 
  | located ( 
      l=binop_or_expr; "||"; r=binop_and_expr; { PBinOp (l, POr, r) }
    )
  | e=binop_and_expr; { e }

let binop_and_expr := 
  | located ( 
      l=binop_and_expr; "&&"; r=binop_cmp_expr; { PBinOp (l, PAnd, r) }
    )
  | e=binop_cmp_expr; { e } 

let binop_cmp_expr := 
  | located ( 
      | l=binop_cmp_expr; "=="; r=binop_add_expr; { PBinOp (l, PEq, r) }
      | l=binop_cmp_expr; "!="; r=binop_add_expr; { PBinOp (l, PNeq, r) }
      | l=binop_cmp_expr; "<="; r=binop_add_expr; { PBinOp (l, PLte, r) }
      | l=binop_cmp_expr; "<"; r=binop_add_expr; { PBinOp (l, PLt, r) }
      | l=binop_cmp_expr; ">"; r=binop_add_expr; { PBinOp (l, PGt, r) }
      | l=binop_cmp_expr; ">="; r=binop_add_expr; { PBinOp (l, PGte, r) }
    )
  | e=binop_add_expr; { e }

let binop_add_expr := 
  | located (
      | l=binop_add_expr; "+"; r=binop_mul_expr; { PBinOp (l, PPlus, r) }
      | l=binop_add_expr; "-"; r=binop_mul_expr; { PBinOp (l, PMinus, r) }
    )
  | e=binop_mul_expr; { e }

let binop_mul_expr := 
  | located (
      | l=binop_mul_expr; "*"; r=unop_expr; { PBinOp (l, PMul, r) }
      | l=binop_mul_expr; "/"; r=unop_expr; { PBinOp (l, PDiv, r) }
      | l=binop_mul_expr; "%"; r=unop_expr; { PBinOp (l, PMod, r) }
    )
  | e=unop_expr; { e }

let unary_op := 
  | "-"; { PNeg }
  | "!"; { PNot }

(* -------------------------------------------------------------------------- *)

(* [located(x)] recognizes the same input fragment as [x] and wraps its
   semantic value of type ['a] as a value of type ['a located]. *)

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
