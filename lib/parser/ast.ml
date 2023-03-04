open Base
open Loc
open Proj

(* ----- Types ----- *)

type raw_typ =
  | ATypNamed of string
  | ATypFun of { farg_types : typ list; ret_typ : typ }
[@@deriving show]

and typ = raw_typ located [@@deriving show]

(* ----- Expressions ----- *)

type unop = ANot [@@deriving show]

type binop =
  | APlus
  | AMinus
  | AMul
  | ADiv
  | AMod
  | ALt
  | ALte
  | AEq
  | ANeq
  | AGte
  | AGt
[@@deriving show]

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
and raw_stmt = ALetStmt of raw_let_decl | AExprStmt of raw_expr
[@@deriving show]

and stmt = raw_stmt located [@@deriving show]

(* ----- Top level ----- *)

type raw_import = { herbarium : string option; path : string list }
[@@deriving show]

type import = raw_import Loc.located [@@deriving show]

type top_decl_raw =
  | AToplevelLet of raw_let_decl
  | AExtern of { name : string; typ : typ; linkname : string }
  | AEntry of block
[@@deriving show]

type top_decl = top_decl_raw located [@@deriving show]

(* ----- Parsed File ----- *)

type parsed_file = { imports : import list; decls : top_decl list }
[@@deriving show]

(* ----- Modules ----- *)

type resolved_import = { imported_cu : cu; import : import } [@@deriving show]

type ast_module = {
  cu : cu;
  resolved_imports : resolved_import list;
  decls : top_decl list;
}
[@@deriving show]

type ast = { entry_module : ast_module; lib_modules : ast_module list }
[@@deriving show]

(* ----- Helpers ------ *)

let equal_ast_module m1 m2 = equal_cu m1.cu m2.cu

module Module_comparator = struct
  type t = ast_module

  let compare x y = Cu_comparator.comparator.compare x.cu y.cu
  let sexp_of_t x = Sexp.Atom (show_cu x.cu)

  include (val Comparator.make ~compare ~sexp_of_t)
end
