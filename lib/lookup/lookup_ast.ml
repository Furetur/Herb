open Base
open Loc
open Ident
open Ast_operators

(* ----- Types ----- *)

type raw_typ =
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

and builtin = LPrint of expr | LAssert of expr

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

and block = stmt list
and expr = raw_expr located

(* ----- Declarations ----- *)
and raw_let_decl = ident * expr

(* ----- Statements ----- *)
and raw_stmt = LLetStmt of raw_let_decl | LExprStmt of expr
and stmt = raw_stmt located

(* ----- Top level ----- *)

type top_decl = raw_let_decl located

(* ----- Modules ----- *)

type lib_module = {
  cu: Proj.cu; 
  decls: top_decl list;
}

type entry = block located
type entry_module = { module_ : lib_module; entry : entry }

(* ----- Lookup Ast ----- *)

type lookup_ast = { lib_modules : lib_module list; entry_module : entry_module }
