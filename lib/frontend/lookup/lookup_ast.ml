open Base
open Loc
open Ident
open Ast_operators

(* ----- Types ----- *)

type raw_typ =
  | LTypUnit
  | LTypString
  | LTypInt
  | LTypBool
  | LTypFun of { farg_types : typ list; ret_typ : typ }
[@@deriving show]

and typ = raw_typ located [@@deriving show]

(* ----- Expressions ----- *)

type literal =
  | LBool of bool
  | LInt of int
  | LString of string
  | LFun of { fargs : (ident * typ) list; body : expr }
[@@deriving show]

and builtin = LPrint of expr | LAssert of expr [@@deriving show]

and raw_expr =
  | LLiteral of literal
  | LBuiltin of builtin
  | LExtern of { typ : typ; linkname : string }
  | LIdent of ident
  | LAssign of expr * expr
  | LUnOp of unop * expr
  | LBinOp of expr * binop * expr
  | LFunCall of { callee : expr; args : expr list }
  | LBlock of block
  | LIf of { cond : expr; then_ : block; else_ : block }
  | LWhile of { cond : expr; body : block }
[@@deriving show]

and block = stmt list [@@deriving show]
and expr = raw_expr located [@@deriving show]

(* ----- Declarations ----- *)
and raw_let_decl = ident * expr [@@deriving show]

(* ----- Statements ----- *)
and raw_stmt = LLetStmt of raw_let_decl | LExprStmt of expr [@@deriving show]
and stmt = raw_stmt located [@@deriving show]

(* ----- Top level ----- *)

type top_decl = raw_let_decl located [@@deriving show]

(* ----- Lookup Ast ----- *)

type entry = block located [@@deriving show]
type raw_lookup_ast = { decls : top_decl list; entry : entry } [@@deriving show]
type lookup_ast = raw_lookup_ast located [@@deriving show]
