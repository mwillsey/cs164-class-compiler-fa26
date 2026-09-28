open S_exp
open Shared.Directive

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

let gensym : string -> string =
  let counter = ref 0 in
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1;
    symbol

let rec compile_exp (exp : s_exp) : directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Lst [ Sym "add1"; l ] -> compile_exp l @ [ Add (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "sub1"; l ] -> compile_exp l @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Lst [ Sym "not"; l ] ->
      compile_exp l @ [ Cmp (Reg Rax, operand_of_bool false) ] @ zf_to_bool
  | Lst [ Sym "zero?"; l ] ->
      compile_exp l @ [ Cmp (Reg Rax, operand_of_num 0) ] @ zf_to_bool
  | Lst [ Sym "num?"; arg ] ->
      compile_exp arg
      @ [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ]
      @ zf_to_bool
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let label_else = gensym "else" in
      let label_then = gensym "then" in
      let label_done = gensym "done" in
      compile_exp e_cond
      @ [ Cmp (Reg Rax, operand_of_bool false); Je label_else ]
      @ [ Label label_then ] @ compile_exp e_then @ [ Jmp label_done ]
      @ [ Label label_else ] @ compile_exp e_else @ [ Label label_done ]
  | Lst [ Sym "+"; a; b ] ->
      compile_exp a
      @ [ Mov (Reg R8, Reg Rax) ]
      @ compile_exp b
      @ [ Add (Reg Rax, Reg R8) ]
  | _ -> failwith "I can't handle that sexp"

let compile (program : s_exp) : directive list =
  let directives = compile_exp program in
  [ Global "entry"; Label "entry" ] @ directives @ [ Ret ]
