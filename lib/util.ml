open String

module Symtab = Map.Make (struct
  type t = string

  let compare = compare
end)

type 'a symtab = 'a Symtab.t

include Symtab

(* get everything from a file *)
let rec input_all (ch : in_channel) : string =
  try
    let c = input_char ch in
    String.make 1 c ^ input_all ch
  with End_of_file -> ""
