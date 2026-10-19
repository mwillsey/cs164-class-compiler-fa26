type t =
  | Num of int
  | Sym of string
  | Chr of char
  | Str of string
  | Lst of t list
  | Dots
[@@deriving show]
