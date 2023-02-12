open Base
open Lexing

type t = Lexing.position * Lexing.position
type 'a located = { loc : t; value : 'a }

let line_col pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
let start_line_col (pos, _) = line_col pos
let end_line_col (_, pos) = line_col pos

let pp_located (f : Formatter.t -> 'a -> unit) (fmt : Formatter.t)
    (x : 'a located) : unit =
  f fmt x.value
