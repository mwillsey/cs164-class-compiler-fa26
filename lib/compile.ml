open S_exp
open Shared.Directive
open Symtab
open Util
open Shared.Error

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00
let bool_shift = 7
let bool_mask = 0b1111111
let bool_tag = 0b0011111
let pair_tag = 0b010
let heap_mask = 0b111

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

let ensure_num op =
  [
    Mov (Reg R8, op);
    And (Reg R8, Imm num_mask);
    Cmp (Reg R8, Imm num_tag);
    Jne "error";
  ]

let ensure_pair op =
  [
    Mov (Reg R8, op);
    And (Reg R8, Imm heap_mask);
    Cmp (Reg R8, Imm pair_tag);
    Jne "error";
  ]

let align_stack_index stack_index =
  if stack_index mod 16 = -8 then stack_index else stack_index - 8

(* compiles the primitive assuming its arguments are already in rax,
   or else on the stack *)
let compile_primitive stack_index = function
  | "add1" -> ensure_num (Reg Rax) @ [ Add (Reg Rax, operand_of_num 1) ]
  | "sub1" -> ensure_num (Reg Rax) @ [ Sub (Reg Rax, operand_of_num 1) ]
  | "not" -> [ Cmp (Reg Rax, operand_of_bool false) ] @ zf_to_bool
  | "zero?" -> [ Cmp (Reg Rax, operand_of_num 0) ] @ zf_to_bool
  | "num?" ->
      [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ] @ zf_to_bool
  | "+" ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_addr stack_index) ]
      @ [ Add (Reg Rax, Reg R8) ]
  | "-" ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, Reg Rax) ]
      @ [ Mov (Reg Rax, stack_addr stack_index) ]
      @ [ Sub (Reg Rax, Reg R8) ]
  | "=" ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_addr stack_index) ]
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | "<" ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, Reg Rax) ]
      @ [ Mov (Reg Rax, stack_addr stack_index) ]
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ lf_to_bool
  | "pair" ->
      [
        Mov (Reg R8, stack_addr stack_index);
        Mov (MemOffset (Reg Rdi, Imm 0), Reg R8);
        Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax);
        Mov (Reg Rax, Reg Rdi);
        Or (Reg Rax, Imm pair_tag);
        Add (Reg Rdi, Imm 16);
      ]
  | "left" ->
      ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag))) ]
  | "right" ->
      ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8))) ]
  | "read-num" ->
      [
        Mov (stack_addr stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "read_num";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_addr stack_index);
      ]
  | "newline" ->
      [
        Mov (stack_addr stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "print_newline";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_addr stack_index);
        Mov (Reg Rax, operand_of_bool true);
      ]
  | "print" ->
      [
        Mov (stack_addr stack_index, Reg Rdi);
        Mov (Reg Rdi, Reg Rax);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "print_value";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_addr stack_index);
        Mov (Reg Rax, operand_of_bool true);
      ]
  | p -> failwith ("unexpected prim " ^ p)

let rec compile_exp (env : int symtab) (stack_index : int) (exp : s_exp) :
    directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      List.concat_map (compile_exp env stack_index) exps
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
  | Lst [ Sym prim ] -> compile_primitive stack_index prim
  | _ -> raise (Stuck exp)

let compile (program : s_exp list) : directive list =
  let defns, body = defns_and_body program in
  let directives = compile_exp Symtab.empty (-8) body in
  [
    Global "entry";
    Extern "error";
    Extern "read_num";
    Extern "print_newline";
    Extern "print_value";
    Label "entry";
  ]
  @ directives @ [ Ret ]
