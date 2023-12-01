open Base
open Label

type ident = string [@@deriving show]

(* ----- Operators ----- *)

type binop =
  | BinopIntPlus
  | BinopIntMinus
  | BinopIntMul
  | BinopIntDiv
  | BinopIntMod

(* ----- Expressions ----- *)

type constant = ConstantInt of int

and expr =
  | Constant of constant
  | Ident of ident
  | Binop of expr * binop * expr
  | Builtin of builtin

and builtin = Print of expr | Println of expr | Assert of expr

(* ----- Statements ----- *)
and lvalue = LvalueIdent of ident
and stmt = Assign of lvalue * expr | ExprStmt of expr

(* ----- Structure ----- *)

type terminator =
  | Jump of label
  | CondBranch of { cond : expr; if_true : label; if_false : label }
  | Return of expr

type basicblock = { label : label; body : stmt list; terminator : terminator }

type func_body = {
  locals : ident list;
  entry_block : basicblock;
  blocks : basicblock list;
}

type ir = { entry : func_body }
