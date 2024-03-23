open Base

(* ----- Operators ----- *)

type binop =
  | BinopPlus
  | BinopMinus
  | BinopMul
  | BinopDiv
  | BinopMod
  | BinopLt
  | BinopLte
  | BinopEq
  | BinopNeq
  | BinopGt
  | BinopGte
[@@deriving show]

(* ----- Expressions ----- *)

type constant = ConstantInt of int [@@deriving show]

and expr =
  | Constant of constant
  | Ident of string
  | Binop of expr * binop * expr
  | Call of { callee : expr; args : expr list }
[@@deriving show]

(* ----- Declarations ----- *)
and let_decl = string * expr [@@deriving show]

(* ----- Statements ----- *)
and stmt =
  | LetDecl of let_decl
  | Assign of expr * expr
  | ExprStmt of expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
[@@deriving show]

and block = stmt list [@@deriving show]

(* ----- Parse Tree ----- *)

type parsetree = { entry : block } [@@deriving show]
