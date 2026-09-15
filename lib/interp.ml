open S_exp


type value = Number of int | Boolean of bool;;

let int_of_value (v: value): int =
  match v with
  Number n -> n
  | Boolean _ -> failwith "it's a boolean"

let rec interp_exp (exp: s_exp): value =
  match exp with
  Num n -> Number n
  | Lst [Sym "add1"; l] -> Number (int_of_value (interp_exp l) + 1)
  | Lst [Sym "sub1"; l] -> Number (int_of_value (interp_exp l) - 1)
  | Sym "true" -> Boolean true
  | Sym "false" -> Boolean false
  | Lst [Sym "not"; l] -> 
    (match interp_exp l with
    Boolean false -> Boolean true
    | _ -> Boolean false)
  | Lst [Sym "num?"; l] -> 
    (match interp_exp l with
    Number _ -> Boolean true
    | _ -> Boolean false)
  | Lst [Sym "zero?"; l] -> 
    if interp_exp l = Number 0 then Boolean true else Boolean false
    (* (match interp_exp l with
    Number 0 -> Boolean true
    | _ -> Boolean false) *)
  | _ -> failwith "I can't handle that sexp"

let string_of_value (v: value): string =
  match v with 
  Number n -> string_of_int n
  | Boolean b -> string_of_bool b

let interp (program: string): string =
  program |> parse |> interp_exp |> string_of_value
