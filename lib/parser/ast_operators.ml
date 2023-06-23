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
  | AOr
  | AAnd
[@@deriving show]

let show_binop = function
  | APlus -> "+"
  | AMinus -> "-"
  | AMul -> "*"
  | ADiv -> "/"
  | AMod -> "%"
  | ALt -> "<"
  | ALte -> "<="
  | AEq -> "=="
  | ANeq -> "!="
  | AGte -> ">="
  | AGt -> ">"
  | AOr -> "||"
  | AAnd -> "&&"
