%token EOF

// Atoms
%token <int> INT
%token <string> ID


// Operators
%token DEF "="
%token PLUS "+" MINUS "-" MUL "*" DIV "/" MOD "%"
%token LT "<" LTE "<=" EQ "==" NEQ "!=" GTE ">=" GT ">"

// Punctuation
%token SEMI ";" LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}" COMMA ","

// Keywords
%token LET ENTRY IF ELSE WHILE RETURN


%{
  open Parsetree
%}

%start <parsetree> herbfile

%%

(* -------------------------------------------------------------------------- *)

let herbfile :=
  ENTRY; b=block; EOF; { { entry=b } }

(* ----- Declarations ----- *)

let let_decl := LET; name=ID; "="; e=expr; SEMI; { (name, e) }

(* ----- Statements ----- *)

let stmt :=
  | ld=let_decl; { LetDecl ld }
  | IF; cond=expr; then_=block; ELSE; else_=block; { If(cond, then_, else_) }
  | WHILE; cond=expr; body=block; { While(cond, body) }
  | l=expr; "="; r=expr; SEMI; { Assign(l, r) }
  | e=expr; SEMI; { ExprStmt e }
  | RETURN; e=expr; SEMI; { Return e }

let block := LBRACE; stmts=stmt*; RBRACE; { stmts }

(* ----- Expressions ----- *)
(* From lower precedence to higher *)

(* -- Expression -- *)

let expr := e=binop_expr; { e }

(* -- Operators -- *)

let binop_expr := binop_cmp_expr

let binop_cmp_expr :=
  | l=binop_cmp_expr; "=="; r=binop_add_expr; { Binop(l, BinopEq, r) }
  | l=binop_cmp_expr; "!="; r=binop_add_expr; { Binop(l, BinopNeq, r) }
  | l=binop_cmp_expr; "<="; r=binop_add_expr; { Binop(l, BinopLte, r) }
  | l=binop_cmp_expr; "<"; r=binop_add_expr; { Binop(l, BinopLt, r) }
  | l=binop_cmp_expr; ">"; r=binop_add_expr; { Binop(l, BinopGt, r) }
  | l=binop_cmp_expr; ">="; r=binop_add_expr; { Binop(l, BinopGte, r) }
  | e=binop_add_expr; { e }

let binop_add_expr :=
  | l=binop_add_expr; "+"; r=binop_mul_expr; { Binop(l, BinopPlus, r) }
  | l=binop_add_expr; "-"; r=binop_mul_expr; { Binop(l, BinopMinus, r) }
  | e=binop_mul_expr; { e }

let binop_mul_expr :=
  | l=binop_mul_expr; "*"; r=operand; { Binop(l, BinopMul, r) }
  | l=binop_mul_expr; "/"; r=operand; { Binop(l, BinopDiv, r) }
  | l=binop_mul_expr; "%"; r=operand; { Binop(l, BinopMod, r) }
  | e=operand; { e }

(* -- Operand -- *)

let operand :=
  | i = INT; { Constant (ConstantInt i) }
  | name=ID; { Ident name }
  | "("; e=expr; ")"; { e }
  | callee=expr; "("; args=separated_list(",", expr); ")"; { Call { callee; args } }

(* -------------------------------------------------------------------------- *)

(* [located(x)] recognizes the same input fragment as [x] and wraps its
   semantic value of type ['a] as a value of type ['a located]. *)

let located(x) ==
  ~ = x; { x $loc }
