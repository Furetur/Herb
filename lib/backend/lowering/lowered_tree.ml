open Ident
open Typ

type typed_ident = ident * typ [@@deriving show]
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
  | LiteralBool of bool
  | LiteralInt of int
  | LiteralString of string
[@@deriving show]

and builtin = BuiltinPrint of expr | BuiltinAssert of expr [@@deriving show]

and raw_expr =
  | Literal of literal
  | Builtin of builtin
  | Ident of typed_ident
  | UnOp of unop * expr
  | BinOp of expr * binop * expr
[@@deriving show]

and expr = raw_expr typed [@@deriving show]

(* ----- Statements ----- *)

type stmt =
  | AssignStmt of typed_ident * expr
  | IfStmt of { cond : expr; then_ : block; else_ : block }
  | WhileStmt of { cond : expr; body : block }

and block = stmt list [@@deriving show]

(* ----- Declarations ----- *)
and let_decl = ident * expr [@@deriving show]

(* ----- Top level ----- *)

type top_decl =
  | FunDecl of {
      args : typed_ident list;
      locals : typed_ident list;
      body : block;
    }
  | ConstantDecl of typed_ident * literal
[@@deriving show]

(* ----- Lowered Tree ----- *)

type entry = block [@@deriving show]
type lowered_tree = { decls : top_decl list; entry : entry } [@@deriving show]
