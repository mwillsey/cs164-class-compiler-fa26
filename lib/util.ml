open String
open S_exp
open Shared.Error

(* get everything from a file *)
let rec input_all (ch : in_channel) : string =
  try
    let c = input_char ch in
    String.make 1 c ^ input_all ch
  with End_of_file -> ""

type defn = { name : string; args : string list; body : s_exp }

let sym = function Sym s -> s | e -> raise (Stuck e)

let defns_and_body (exps : s_exp list) : defn list * s_exp =
  let rec args_and_rest args = function
    | [] -> (List.rev args, None)
    | [ arg; Dots ] -> (List.rev args, Some (sym arg))
    | arg :: exps -> args_and_rest (sym arg :: args) exps
  in
  let get_defn = function
    | Lst [ Sym "define"; Lst (Sym name :: args); body ] ->
        let args, rest = args_and_rest [] args in
        assert (rest = None);
        { name; args; body }
    | e -> raise (Stuck e)
  in
  let rec go exps defns =
    match exps with
    | [ e ] -> (List.rev defns, e)
    | d :: exps -> go exps (get_defn d :: defns)
    | _ -> raise (Stuck (Sym "empty"))
  in
  go exps []

let is_defn defns name = List.exists (fun d -> d.name = name) defns
let get_defn defns name = List.find (fun d -> d.name = name) defns
