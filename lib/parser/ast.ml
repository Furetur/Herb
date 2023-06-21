open Base
open Loc
open Ast_operators

(* ----- Types ----- *)

type raw_typ =
  | ATypNamed of string
  | ATypFun of { farg_types : typ list; ret_typ : typ }
[@@deriving show]

and typ = raw_typ located [@@deriving show]

(* ----- Expressions ----- *)

type literal =
  | ABool of bool
  | AInt of int
  | AString of string
  | AFun of { fargs : (string * typ) list; body : expr }
[@@deriving show]

and raw_expr =
  | ALiteral of literal
  | AIdent of string
  | AAssign of expr * expr
  | AUnOp of unop * expr
  | ABinOp of expr * binop * expr
  | AFunCall of { callee : expr; args : expr list }
  | ABlock of block
  | AIf of { cond : expr; then_ : block; else_ : block }
  | AWhile of { cond : expr; body : block }
[@@deriving show]

and block = stmt list [@@deriving show]
and expr = raw_expr located [@@deriving show]

(* ----- Declarations ----- *)
and raw_let_decl = string * expr [@@deriving show]

(* ----- Statements ----- *)
and raw_stmt = ALetStmt of raw_let_decl | AExprStmt of expr [@@deriving show]
and stmt = raw_stmt located [@@deriving show]

(* ----- Top level ----- *)

type raw_import = { herbarium : string option; path : string list }
[@@deriving show]

type import = raw_import Loc.located [@@deriving show]

type raw_extern = { name : string; typ : typ; linkname : string }
[@@deriving show]

type top_decl_raw =
  | AToplevelLet of raw_let_decl
  | AExtern of raw_extern
  | AEntry of block
[@@deriving show]

type top_decl = top_decl_raw located [@@deriving show]

(* ----- Parsed File ----- *)

type raw_ast = { decls : top_decl list } [@@deriving show]
type ast = raw_ast located [@@deriving show]
