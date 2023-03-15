open Base
open Lexing

type loc = Lexing.position * Lexing.position
type 'a located = { loc : loc; value : 'a }

let locate loc ~value = { loc; value }

let bad_loc =
  let bad_pos =
    { pos_fname = "error"; pos_lnum = -1; pos_cnum = -1; pos_bol = -1 }
  in
  (bad_pos, bad_pos)

let infile_loc path : loc =
  let pos =
    {
      pos_fname = Fpath.to_string path;
      pos_lnum = -1;
      pos_cnum = -1;
      pos_bol = -1;
    }
  in
  (pos, pos)

let start_loc =
  let start_loc = { pos_fname = ""; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 } in
  (start_loc, start_loc)

let copy_loc { loc; _ } x = { loc; value = x }
let bad_located x = { loc = bad_loc; value = x }
let line_col pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
let start_line_col (pos, _) = line_col pos
let end_line_col (_, pos) = line_col pos
let filename (pos, _) : string = pos.pos_fname

let pp_located (f : Formatter.t -> 'a -> unit) (fmt : Formatter.t)
    (x : 'a located) : unit =
  f fmt x.value
