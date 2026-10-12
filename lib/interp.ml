open S_exp
open Util

type value = Number of int | Boolean of bool | Pair of value * value

let int_of_value (v : value) : int =
  match v with Number n -> n | _ -> failwith "not a number"

let rec interp_exp (env : value symtab) (exp : s_exp) : value =
  match exp with
  | Num n -> Number n
  | Lst [ Sym "add1"; l ] -> Number (int_of_value (interp_exp env l) + 1)
  | Lst [ Sym "sub1"; l ] -> Number (int_of_value (interp_exp env l) - 1)
  | Sym "true" -> Boolean true
  | Sym "false" -> Boolean false
  | Sym x -> Symtab.find x env
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      let e_value = interp_exp env e in
      interp_exp (Symtab.add var e_value env) body
  | Lst [ Sym "not"; l ] -> Boolean (interp_exp env l = Boolean false)
  | Lst [ Sym "zero?"; l ] -> Boolean (interp_exp env l = Number 0)
  | Lst [ Sym "num?"; l ] -> (
      match interp_exp env l with
      | Number _ -> Boolean true
      | _ -> Boolean false)
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let v_cond = interp_exp env e_cond in
      if v_cond = Boolean false then interp_exp env e_else
      else interp_exp env e_then
  | Lst [ Sym "+"; a; b ] ->
      let va = interp_exp env a in
      let vb = interp_exp env b in
      Number (int_of_value va + int_of_value vb)
  | Lst [ Sym "-"; a; b ] ->
      let va = interp_exp env a in
      let vb = interp_exp env b in
      Number (int_of_value va - int_of_value vb)
  | Lst [ Sym "<"; a; b ] ->
      let va = interp_exp env a in
      let vb = interp_exp env b in
      Boolean (int_of_value va < int_of_value vb)
  | Lst [ Sym "="; a; b ] ->
      let va = interp_exp env a in
      let vb = interp_exp env b in
      Boolean (va = vb)
  | Lst [ Sym "pair"; a; b ] ->
      let v1, v2 = (interp_exp env a, interp_exp env b) in
      Pair (v1, v2)
  | Lst [ Sym "left"; e ] -> (
      match interp_exp env e with
      | Pair (v1, _) -> v1
      | _ -> failwith "expected a pair")
  | Lst [ Sym "right"; e ] -> (
      match interp_exp env e with
      | Pair (_, v2) -> v2
      | _ -> failwith "expected a pair")
  | _ -> failwith "I can't handle that sexp"

let rec string_of_value (v : value) : string =
  match v with
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Pair (v1, v2) ->
      Printf.sprintf "(pair %s %s)" (string_of_value v1) (string_of_value v2)

let interp (program : s_exp) : string =
  program |> interp_exp Symtab.empty |> string_of_value
