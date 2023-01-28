open Loc

let pp_located (f: Format.formatter -> 'a -> unit) (fmt: Format.formatter) (x: 'a located): unit =
  f fmt x.value

type unop =
  | PNeg
  [@@ deriving show]


type binop =
  | PPlus | PMinus | PMul | PDiv
  [@@ deriving show]


type expr =
  raw_expr located
  [@@ deriving show]


and raw_expr =
| PInt of int
| PUnOp of unop * expr
| PBinOp of expr * binop * expr
[@@ deriving show]


type raw_decl = PEntry of expr
[@@ deriving show]

type decl = raw_decl located
[@@ deriving show]

type prog = decl list
[@@ deriving show]
