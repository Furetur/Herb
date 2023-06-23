open Base
open Loc
open Ident
open Ast_operators
open Typ

(* ----- Expressions ----- *)

type literal =
  | TBool of bool
  | TInt of int
  | TString of string
  | TFun of { fargs : (ident * typ) list; body : expr }
[@@deriving show]

and builtin = TPrint of expr | TAssert of expr [@@deriving show]

and raw_expr =
  | TLiteral of literal
  | TBuiltin of builtin
  | TExtern of { typ : typ; linkname : string }
  | TIdent of ident
  | TAssign of expr * expr
  | TUnOp of unop * expr
  | TBinOp of expr * binop * expr
  | TFunCall of { callee : expr; args : expr list }
  | TBlock of block
  | TIf of { cond : expr; then_ : block; else_ : block }
  | TWhile of { cond : expr; body : block }
[@@deriving show]

and block = stmt list [@@deriving show]
and expr = raw_expr typed located [@@deriving show]

(* ----- Declarations ----- *)
and raw_let_decl = ident * expr [@@deriving show]

(* ----- Statements ----- *)
and raw_stmt = TLetStmt of raw_let_decl | TExprStmt of expr [@@deriving show]
and stmt = raw_stmt located [@@deriving show]

(* ----- Top level ----- *)

type top_decl = raw_let_decl located [@@deriving show]

(* ----- Typed Ast ----- *)

type entry = block located [@@deriving show]
type raw_typed_ast = { decls : top_decl list; entry : entry } [@@deriving show]
type typed_ast = raw_typed_ast located [@@deriving show]
