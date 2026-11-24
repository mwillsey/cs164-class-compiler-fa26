open Ast
open Util

type value =
  | Number of int
  | Boolean of bool
  | Pair of value * value
  | Function of (string * value Symtab.t)

type environment = value Symtab.t

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
  | Function _ -> "<function>"

let interp_prim0 = function
  | ReadNum -> Number (input_line !input_channel |> int_of_string)
  | Newline ->
      output_string !output_channel "\n";
      Boolean true

let interp_prim1 prim v =
  match prim with
  | Add1 -> Number (int_of_value v + 1)
  | Sub1 -> Number (int_of_value v - 1)
  | Not -> Boolean (v = Boolean false)
  | IsZero -> Boolean (v = Number 0)
  | IsNum -> ( match v with Number _ -> Boolean true | _ -> Boolean false)
  | Left -> (
      match v with Pair (v1, _) -> v1 | _ -> failwith "expected a pair")
  | Right -> (
      match v with Pair (_, v2) -> v2 | _ -> failwith "expected a pair")
  | Print ->
      output_string !output_channel (string_of_value v);
      Boolean true
  | IsPair | IsEmpty -> failwith "I can't handle that sexp"

let interp_prim2 prim va vb =
  match prim with
  | Plus -> Number (int_of_value va + int_of_value vb)
  | Minus -> Number (int_of_value va - int_of_value vb)
  | Lt -> Boolean (int_of_value va < int_of_value vb)
  | Eq -> Boolean (int_of_value va = int_of_value vb)
  | Pair -> Pair (va, vb)

let rec interp_exp (defns : defn list) (env : environment) (exp : expr) : value
    =
  match exp with
  | Num n -> Number n
  | True -> Boolean true
  | False -> Boolean false
  | Var x when Symtab.mem x env -> Symtab.find x env
  | Var x when is_defn defns x -> Function (x, Symtab.empty)
  | Closure f -> Function (f, env)
  | Call (f, args) -> (
      let fv = interp_exp defns env f in
      match fv with
      | Function (f, closed_env) ->
          let defn = get_defn defns f in
          if List.length args = List.length defn.args then
            let vals = List.map (interp_exp defns env) args in
            let fenv =
              Symtab.add_seq
                (List.combine defn.args vals |> List.to_seq)
                closed_env
            in
            interp_exp defns fenv defn.body
          else failwith "wrong num of args"
      | _ -> failwith "not a function")
  | Let (var, e, body) ->
      let e_value = interp_exp defns env e in
      interp_exp defns (Symtab.add var e_value env) body
  | If (e_cond, e_then, e_else) ->
      let v_cond = interp_exp defns env e_cond in
      if v_cond = Boolean false then interp_exp defns env e_else
      else interp_exp defns env e_then
  | Prim0 prim -> interp_prim0 prim
  | Prim1 (prim, e) ->
      let v = interp_exp defns env e in
      interp_prim1 prim v
  | Prim2 (prim, a, b) ->
      let va = interp_exp defns env a in
      let vb = interp_exp defns env b in
      interp_prim2 prim va vb
  | Do exps when List.length exps > 0 ->
      let rev_exps = List.rev_map (interp_exp defns env) exps in
      List.hd rev_exps
  | Lambda _ -> failwith "lambdas should be desugared"
  | _ -> failwith "I can't handle that sexp"

let interp (program : program) : unit =
  (* let program = Constant_fold.fold_program program in
  let s_exp_of_defn (d : defn) =
    S_exp.Lst
      [
        S_exp.Sym "define";
        S_exp.Lst (S_exp.Sym d.name :: List.map (fun a -> S_exp.Sym a) d.args);
        s_exp_of_expr d.body;
      ]
  in
  print_string
    (List.map s_exp_of_defn program.defns @ [ s_exp_of_expr program.body ]
    |> List.map S_exp.string_of_s_exp
    |> String.concat "\n"); *)
  interp_exp program.defns Symtab.empty program.body |> ignore

(** [interp_io prog input] evaluates the program [prog] using [interp_expr],
    reading input from the string [input] and returning the output as a string.
*)
let interp_io (prog : program) input =
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
