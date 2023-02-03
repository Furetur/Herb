open Loc

(* -----    Types    ----- *)

type raw_typ = PTypNamed of string | PTypFun of signature [@@deriving show]

and signature = {arg_types: typ list; ret_typ: typ} [@@deriving show]

and typ = raw_typ located [@@deriving show]

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
  | PString of string
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
  | PFor of { i : string; start_ : expr; end_ : expr; body : expr_block }
[@@deriving show]

and expr_block = expr list
and formal_arg = string * typ
and fun_literal = { formal_args : formal_arg list; body : expr }

(* ----- Declarations ----- *)
and raw_let_decl = string * expr [@@deriving show]

type raw_top_decl =
  | PEntry of expr_block
  | PToplevelLet of raw_let_decl
  | PExtern of { name : string; typ : typ; linkname : string }
[@@deriving show]

type top_decl = raw_top_decl located [@@deriving show]

(* ----- Imports ----- *)

type raw_import = { herbarium : string option; path : string list } [@@deriving show]
type import = raw_import located [@@deriving show]

(* ----- File ----- *)

type herbfile = { imports : import list; decls : top_decl list }
[@@deriving show]
