open S_exp

type value = Number of int | Boolean of bool

let int_of_value (v : value) : int =
  match v with Number n -> n | Boolean _ -> failwith "it's a boolean"

let rec interp_exp (exp : s_exp) : value =
  match exp with
  | Num n -> Number n
  | Lst [ Sym "add1"; l ] -> Number (int_of_value (interp_exp l) + 1)
  | Lst [ Sym "sub1"; l ] -> Number (int_of_value (interp_exp l) - 1)
  | Sym "true" -> Boolean true
  | Sym "false" -> Boolean false
  | Lst [ Sym "not"; l ] -> Boolean (interp_exp l = Boolean false)
  | Lst [ Sym "zero?"; l ] -> Boolean (interp_exp l = Number 0)
  | Lst [ Sym "num?"; l ] -> (
      match interp_exp l with Number _ -> Boolean true | _ -> Boolean false)
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let v_cond = interp_exp e_cond in
      if v_cond = Boolean false then interp_exp e_else else interp_exp e_then
  | Lst [ Sym "+"; a; b ] ->
      let va = interp_exp a in
      let vb = interp_exp b in
      Number (int_of_value va + int_of_value vb)
  | Lst [ Sym "-"; a; b ] ->
      let va = interp_exp a in
      let vb = interp_exp b in
      Number (int_of_value va - int_of_value vb)
  | Lst [ Sym "<"; a; b ] ->
      let va = interp_exp a in
      let vb = interp_exp b in
      Boolean (int_of_value va < int_of_value vb)
  | Lst [ Sym "="; a; b ] ->
      let va = interp_exp a in
      let vb = interp_exp b in
      Boolean (va = vb)
  | _ -> failwith "I can't handle that sexp"

let string_of_value (v : value) : string =
  match v with Number n -> string_of_int n | Boolean b -> string_of_bool b

let interp (program : s_exp) : string = program |> interp_exp |> string_of_value
