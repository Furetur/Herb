open Loc

let pp_located (f : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (x : 'a located) : unit =
  f fmt x.value

type unop = PNeg [@@deriving show]
type binop = PPlus | PMinus | PMul | PDiv [@@deriving show]

type expr = raw_expr located [@@deriving show]

and raw_expr =
  | PInt of int
  | PUnOp of unop * expr
  | PBinOp of expr * binop * expr
  | PExprBlock of expr_block
  | PLet of raw_let_decl
[@@deriving show]

and expr_block = expr list

and raw_let_decl = string * expr
[@@deriving show]

type raw_top_decl = 
  | PEntry of expr_block
  | PToplevelLet of raw_let_decl
[@@deriving show]

type top_decl = raw_top_decl located [@@deriving show]
type herbfile = top_decl list [@@deriving show]
