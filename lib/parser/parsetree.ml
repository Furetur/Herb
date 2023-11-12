open Base

(* ----- Operators ----- *)

type binop =
  | BinopPlus
  | BinopMinus
  | BinopMul
  | BinopDiv
  | BinopMod
[@@deriving show]

(* ----- Expressions ----- *)

type ident = string [@@deriving show]

type constant =
  | ConstantInt of int
[@@deriving show]

and expr =
  | Constant of constant
  | Ident of ident
  | Binop of expr * binop * expr
[@@deriving show]


(* ----- Declarations ----- *)
and let_decl = ident * expr [@@deriving show]

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

type parsetree = { entry: block } [@@deriving show]
