type loc = Lexing.position * Lexing.position

type 'a located = { loc : loc; value : 'a }

let pp_located (f : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (x : 'a located) : unit =
  f fmt x.value

