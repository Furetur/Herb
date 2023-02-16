(* -----    Types    ----- *)

type raw_typ = PTypNamed of string | PTypFun of signature [@@deriving show]
and signature = { arg_types : typ list; ret_typ : typ } [@@deriving show]
and typ = raw_typ Loc.located [@@deriving show]

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

type expr = raw_expr Loc.located [@@deriving show]

and raw_expr =
  | PInt of int
  | PString of string
  | PIdent of string
  | PFunLiteral of fun_literal
  | PAssign of expr * expr
  | PUnOp of unop * expr
  | PBinOp of expr * binop * expr
  | PFunCall of { callee : expr; args : expr list }
  | PBlock of block
  | PIf of { cond : expr; then_ : block; else_ : block }
  | PWhile of { cond : expr; body : block }
  | PFor of { i : string; start_ : expr; end_ : expr; body : block }
[@@deriving show]

and block = stmt list
and formal_arg = string * typ
and fun_literal = { formal_args : formal_arg list; body : expr }

(* ----- Declarations ----- *)
and raw_let_decl = string * expr [@@deriving show]

(* ----- Statements ----- *)
and raw_stmt =
  | PLet of raw_let_decl
  | PExprStmt of raw_expr
  [@@deriving show]

and stmt = raw_stmt Loc.located [@@deriving show]

(* ----- Top level ----- *)
type raw_top_decl =
  | PEntry of block
  | PToplevelLet of raw_let_decl
  | PExtern of { name : string; typ : typ; linkname : string }
[@@deriving show]

type top_decl = raw_top_decl Loc.located [@@deriving show]

(* - Imports - *)

type raw_import = { herbarium : string option; path : string list }
[@@deriving show]

type import = raw_import Loc.located [@@deriving show]

(* ----- File ----- *)

type herbfile = { imports : import list; decls : top_decl list }
[@@deriving show]
