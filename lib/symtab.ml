open String

module Symtab = Map.Make (struct
  type t = string

  let compare = compare
end)

type 'a symtab = 'a Symtab.t

let of_list l = l |> List.to_seq |> Symtab.of_seq

let add_list (tab : 'a symtab) (l : (string * 'a) list) : 'a symtab =
  Symtab.add_seq (List.to_seq l) tab

include Symtab
