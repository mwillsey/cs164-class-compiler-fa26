open S_exp
open Symtab
open Util

type value = Number of int | Boolean of bool | Pair of value * value

let int_of_value (v : value) : int =
  match v with Number n -> n | _ -> failwith "not a number"

let input_channel = ref stdin
let output_channel = ref stdout

let rec string_of_value (v : value) : string =
  match v with
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Pair (v1, v2) ->
      Printf.sprintf "(pair %s %s)" (string_of_value v1) (string_of_value v2)

let rec interp_exp (defns : defn list) (env : value symtab) (exp : s_exp) :
    value =
  match exp with
  | Num n -> Number n
  | Lst [ Sym "add1"; l ] -> Number (int_of_value (interp_exp defns env l) + 1)
  | Lst [ Sym "sub1"; l ] -> Number (int_of_value (interp_exp defns env l) - 1)
  | Sym "true" -> Boolean true
  | Sym "false" -> Boolean false
  | Sym x -> Symtab.find x env
  | Lst (Sym f :: args) when is_defn defns f ->
      let defn = get_defn defns f in
      if List.length args = List.length defn.args then
        let vals = List.map (interp_exp defns env) args in
        let fenv = (List.combine defn.args vals) |> List.to_seq |> Symtab.of_seq in
        interp_exp defns fenv defn.body
      else failwith "wrong num of args"
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      let e_value = interp_exp defns env e in
      interp_exp defns (Symtab.add var e_value env) body
  | Lst [ Sym "not"; l ] -> Boolean (interp_exp defns env l = Boolean false)
  | Lst [ Sym "zero?"; l ] -> Boolean (interp_exp defns env l = Number 0)
  | Lst [ Sym "num?"; l ] -> (
      match interp_exp defns env l with
      | Number _ -> Boolean true
      | _ -> Boolean false)
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let v_cond = interp_exp defns env e_cond in
      if v_cond = Boolean false then interp_exp defns env e_else
      else interp_exp defns env e_then
  | Lst [ Sym "+"; a; b ] ->
      let va = interp_exp defns env a in
      let vb = interp_exp defns env b in
      Number (int_of_value va + int_of_value vb)
  | Lst [ Sym "-"; a; b ] ->
      let va = interp_exp defns env a in
      let vb = interp_exp defns env b in
      Number (int_of_value va - int_of_value vb)
  | Lst [ Sym "<"; a; b ] ->
      let va = interp_exp defns env a in
      let vb = interp_exp defns env b in
      Boolean (int_of_value va < int_of_value vb)
  | Lst [ Sym "="; a; b ] ->
      let va = interp_exp defns env a in
      let vb = interp_exp defns env b in
      Boolean (int_of_value va = int_of_value vb)
  | Lst [ Sym "pair"; a; b ] ->
      let v1 = interp_exp defns env a in
      let v2 = interp_exp defns env b in
      Pair (v1, v2)
  | Lst [ Sym "left"; e ] -> (
      match interp_exp defns env e with
      | Pair (v1, _) -> v1
      | _ -> failwith "expected a pair")
  | Lst [ Sym "right"; e ] -> (
      match interp_exp defns env e with
      | Pair (_, v2) -> v2
      | _ -> failwith "expected a pair")
  | Lst [ Sym "read-num" ] -> Number (input_line !input_channel |> int_of_string)
  | Lst [ Sym "print"; e ] ->
      let v = interp_exp defns env e in
      output_string !output_channel (string_of_value v);
      Boolean true
  | Lst [ Sym "newline" ] ->
      output_string !output_channel "\n";
      Boolean true
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      let rev_exps = List.rev_map (interp_exp defns env) exps in
      List.hd rev_exps
  | _ -> failwith "I can't handle that sexp"

let interp (program : s_exp list) : unit =
  let defns, body = defns_and_body program in
  interp_exp defns Symtab.empty body |> ignore

(** [interp_io prog input] evaluates the program [prog] using [interp_expr],
    reading input from the string [input] and returning the output as a string.
*)
let interp_io (prog : s_exp list) input =
  let input_pipe_ex, input_pipe_en = Unix.pipe () in
  let output_pipe_ex, output_pipe_en = Unix.pipe () in
  input_channel := Unix.in_channel_of_descr input_pipe_ex;
  set_binary_mode_in !input_channel false;
  output_channel := Unix.out_channel_of_descr output_pipe_en;
  set_binary_mode_out !output_channel false;
  let write_input_channel = Unix.out_channel_of_descr input_pipe_en in
  set_binary_mode_out write_input_channel false;
  let read_output_channel = Unix.in_channel_of_descr output_pipe_ex in
  set_binary_mode_in read_output_channel false;
  output_string write_input_channel input;
  close_out write_input_channel;
  interp prog;
  close_out !output_channel;
  let r = input_all read_output_channel in
  input_channel := stdin;
  output_channel := stdout;
  r
