open S_exp
open Shared.Directive
open Util

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00
let bool_shift = 7
let bool_mask = 0b1111111
let bool_tag = 0b0011111

let operand_of_bool (b : bool) : operand =
  Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let operand_of_num (n : int) : operand = Imm ((n lsl num_shift) lor num_tag)

let zf_to_bool : directive list =
  [
    (* zero out rax *)
    Mov (Reg Rax, Imm 0);
    (* 1 if ZF is set, 0 otherwise *)
    Setz (Reg Rax);
    (* rax << bool_shift *)
    Shl (Reg Rax, Imm bool_shift);
    (* tag rax as a boolean: rax = rax | bool_tag *)
    Or (Reg Rax, Imm bool_tag);
  ]

let lf_to_bool : directive list =
  [
    Mov (Reg Rax, Imm 0);
    Setl (Reg Rax);
    Shl (Reg Rax, Imm bool_shift);
    Or (Reg Rax, Imm bool_tag);
  ]

let gensym : string -> string =
  let counter = ref 0 in
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1;
    symbol

let stack_addr stack_index = MemOffset (Reg Rsp, Imm stack_index)

(* compiles the primitive assuming its arguments are already in rax *)
let compile_primitive stack_index = function
  | "add1" -> [ Add (Reg Rax, operand_of_num 1) ]
  | "sub1" -> [ Sub (Reg Rax, operand_of_num 1) ]
  | "not" -> [ Cmp (Reg Rax, operand_of_bool false) ] @ zf_to_bool
  | "zero?" -> [ Cmp (Reg Rax, operand_of_num 0) ] @ zf_to_bool
  | "num?" ->
      [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ] @ zf_to_bool
  | "+" -> [ Mov (Reg R8, stack_addr stack_index) ] @ [ Add (Reg Rax, Reg R8) ]
  | "-" ->
      [ Mov (Reg R8, Reg Rax) ]
      @ [ Mov (Reg Rax, stack_addr stack_index) ]
      @ [ Sub (Reg Rax, Reg R8) ]
  | "=" ->
      [ Mov (Reg R8, stack_addr stack_index) ]
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | "<" ->
      [ Mov (Reg R8, Reg Rax) ]
      @ [ Mov (Reg Rax, stack_addr stack_index) ]
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ lf_to_bool
  | p -> failwith ("unexpected prim " ^ p)

let rec compile_exp (env : int symtab) (stack_index : int) (exp : s_exp) :
    directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let label_else = gensym "else" in
      let label_then = gensym "then" in
      let label_done = gensym "done" in
      compile_exp env stack_index e_cond
      @ [ Cmp (Reg Rax, operand_of_bool false); Je label_else ]
      @ [ Label label_then ]
      @ compile_exp env stack_index e_then
      @ [ Jmp label_done ] @ [ Label label_else ]
      @ compile_exp env stack_index e_else
      @ [ Label label_done ]
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      compile_exp env stack_index e
      @ [ Mov (stack_addr stack_index, Reg Rax) ]
      @ compile_exp (Symtab.add var stack_index env) (stack_index - 8) body
  | Sym var -> [ Mov (Reg Rax, stack_addr (Symtab.find var env)) ]
  | Lst [ Sym prim; a; b ] ->
      compile_exp env stack_index a
      @ [ Mov (stack_addr stack_index, Reg Rax) ]
      @ compile_exp env (stack_index - 8) b
      @ compile_primitive stack_index prim
  | Lst [ Sym prim; arg ] ->
      compile_exp env stack_index arg @ compile_primitive stack_index prim
  | _ -> failwith "I can't handle that sexp"

let compile (program : s_exp) : directive list =
  let directives = compile_exp Symtab.empty (-8) program in
  [ Global "entry"; Label "entry" ] @ directives @ [ Ret ]
