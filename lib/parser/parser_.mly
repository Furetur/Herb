%token EOF

// Atoms
%token <int> INT
%token <string> ID
%token <string> STRING


// Operators 
%token DEF "="
%token ASSIGN ":="
%token PLUS "+" MINUS "-" MUL "*" DIV "/" MOD "%"
%token OR "||" AND "&&" NOT "!"
%token LT "<" LTE "<=" EQ "==" NEQ "!=" GTE ">=" GT ">"

// Punctuation
%token COMMA "," COLON ":" ARROW "->" LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}" DOT "." DOTDOT ".."

// Keywords
%token LET ENTRY IF ELSE WHILE IMPORT FOR EXTERN


%{ 
  open Ast
  open Make_ast
%}

%start <parsed_file> herbfile  

%%

(* -------------------------------------------------------------------------- *)

let herbfile :=
  i=import*; d=toplevel_decl*; EOF; { {imports=i; decls=d } }

(* -----   Imports   ----- *)

let raw_import :=
  | IMPORT; herbarium=ID; ":"; path=separated_nonempty_list(".", ID); { { herbarium = Some herbarium; path } }
  | IMPORT; path=separated_nonempty_list(".", ID); { { herbarium = None; path } }

let import := located(
  | x=raw_import; { make_import x }
)


(* -----    Types    ----- *)

let typ := located(
  | name=ID; { make_typ_named name }
  | "("; arg_types=separated_list(",", typ); ")"; "->"; ret_typ=typ; { make_typ_fun arg_types ret_typ }
) 

(* ----- Declarations ----- *)

let raw_let_decl := LET; name=ID; "="; e=expr; { make_raw_let_decl name e }

let toplevel_decl :=
  located (
    | ENTRY; e=block; { make_entry e }
    | d=raw_let_decl; { make_top_level_let d }
    | EXTERN; name=ID; ":"; typ=typ; "="; linkname=STRING; { make_extern name typ linkname }
  )

(* ----- Statements ----- *)

let stmt := 
  | located( 
      d=raw_let_decl; { make_let_stmt d }
    )
  | e=expr; { make_expr_stmt e }

(* ----- Expressions ----- *)

(* -- Operand -- *)

let formal_arg := name=ID; ":"; t=typ; { (name, t) }
let formal_args := args=separated_list(",", formal_arg); { args }

let operand := 
  | located(
      | i = INT; { make_int i }
      | s = STRING; { make_string s }
      | name=ID; { make_ident name }
      | "("; formal_args=formal_args; ")"; "->"; body=expr; { make_fun_literal formal_args body }
    )
  | "("; e=expr; ")"; { e }

(* -- Primary Expression -- *)

let arguments := exprs=separated_list(",", expr); { exprs }

let primary_expr := 
  | x=operand; { x }
  | located( 
    callee=primary_expr; "("; args=arguments; ")"; { make_fun_call callee args } 
  )
  
(* -- Expression -- *)

let expr := 
  | e=binop_expr; { e }
  | located(   
    | exprs=block; { make_block exprs }
    | IF; cond=expr; then_=block; { make_if cond then_ [] }
    | IF; cond=expr; then_=block; ELSE; else_=block; { make_if cond then_ else_ }
    | WHILE; cond=expr; body=block; { make_while cond body }
    | FOR; i=ID; "="; start_=expr; ".."; end_=expr; body=block; { make_for i start_ end_ body } 
  )

let block := LBRACE; exprs=stmt*; RBRACE; { exprs }

(* -- Operators -- *)

let unop_expr := 
  | e=primary_expr; { e }
  | located( 
    op=unary_op; e=unop_expr; { make_unop op e } 
  )

(* From lower precedence to higher *)

let binop_expr := binop_assign_expr

let binop_assign_expr := 
  | located ( 
      l=binop_or_expr; ":="; r=binop_assign_expr; { make_assign l r } 
    )
  | e=binop_or_expr; { e }

let binop_or_expr := 
  | located ( 
      l=binop_or_expr; "||"; r=binop_and_expr; { make_or l r }
    )
  | e=binop_and_expr; { e }

let binop_and_expr := 
  | located ( 
      l=binop_and_expr; "&&"; r=binop_cmp_expr; { make_and l r }
    )
  | e=binop_cmp_expr; { e } 

let binop_cmp_expr := 
  | located ( 
      | l=binop_cmp_expr; "=="; r=binop_add_expr; { make_eq l r }
      | l=binop_cmp_expr; "!="; r=binop_add_expr; { make_neq l r }
      | l=binop_cmp_expr; "<="; r=binop_add_expr; { make_lte l r }
      | l=binop_cmp_expr; "<"; r=binop_add_expr; { make_lt l r }
      | l=binop_cmp_expr; ">"; r=binop_add_expr; { make_gt l r }
      | l=binop_cmp_expr; ">="; r=binop_add_expr; { make_gte l r }
    )
  | e=binop_add_expr; { e }

let binop_add_expr := 
  | located (
      | l=binop_add_expr; "+"; r=binop_mul_expr; { make_plus l r }
      | l=binop_add_expr; "-"; r=binop_mul_expr; { make_minus l r }
    )
  | e=binop_mul_expr; { e }

let binop_mul_expr := 
  | located (
      | l=binop_mul_expr; "*"; r=unop_expr; { make_mul l r }
      | l=binop_mul_expr; "/"; r=unop_expr; { make_div l r }
      | l=binop_mul_expr; "%"; r=unop_expr; { make_mod l r }
    )
  | e=unop_expr; { e }

let unary_op := 
  | "!"; { Ast.ANot }

(* -------------------------------------------------------------------------- *)

(* [located(x)] recognizes the same input fragment as [x] and wraps its
   semantic value of type ['a] as a value of type ['a located]. *)

let located(x) ==
  ~ = x; { x $loc }
