open Loc

(* -----    Types    ----- *)

type raw_typ = PTypNamed of string [@@deriving show]
type typ = raw_typ located [@@deriving show]

(* ----- Expressions ----- *)

type unop = PNeg | PNot [@@deriving show]

type binop =
  | PPlus
  | PMinus
  | PMul
  | PDiv
  | PMod
  | POr
  | PAnd
  | PLt
  | PLte
  | PEq
  | PNeq
  | PGte
  | PGt
[@@deriving show]

type expr = raw_expr located [@@deriving show]

and raw_expr =
  | PInt of int
  | PIdent of string
  | PFunLiteral of fun_literal
  | PAssign of expr * expr
  | PUnOp of unop * expr
  | PBinOp of expr * binop * expr
  | PFunCall of { callee : expr; args : expr list }
  | PExprBlock of expr_block
  | PLet of raw_let_decl
  | PIf of { cond : expr; then_ : expr_block; else_ : expr_block }
  | PWhile of { cond : expr; body : expr_block }
[@@deriving show]

and expr_block = expr list
and formal_arg = string * typ
and fun_literal = { formal_args : formal_arg list; body : expr }

(* ----- Declarations ----- *)
and raw_let_decl = string * expr [@@deriving show]

type raw_top_decl = PEntry of expr_block | PToplevelLet of raw_let_decl
[@@deriving show]

type top_decl = raw_top_decl located [@@deriving show]

(* ----- File ----- *)

type herbfile = top_decl list [@@deriving show]
