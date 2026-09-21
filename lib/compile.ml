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

let operand_of_num (n : int) : operand =
  Imm ((n lsl num_shift) lor num_tag)

let zf_to_bool: directive list = [
  Mov (Reg Rax, Imm 0);          (* zero out rax *)
  Setz (Reg Rax);                (* 1 if ZF is set, 0 otherwise *)
  Shl (Reg Rax, Imm bool_shift); (* rax << bool_shift *)
  Or (Reg Rax, Imm bool_tag)     (* tag rax as a boolean: rax = rax | bool_tag *)
]


let rec compile_exp (exp: s_exp): directive list =
  match exp with
  | Num n               -> [Mov (Reg Rax, operand_of_num n)]
  | Lst [Sym "add1"; l] -> compile_exp l @ [ Add (Reg Rax, operand_of_num 1) ]
  | Lst [Sym "sub1"; l] -> compile_exp l @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Sym "true"  -> [ Mov (Reg Rax, operand_of_bool true)]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false)]
  | Lst [Sym "not"; l]   -> compile_exp l @ [ Cmp (Reg Rax, operand_of_bool false) ] @ zf_to_bool
  | Lst [Sym "zero?"; l] -> compile_exp l @ [ Cmp (Reg Rax, operand_of_num 0) ] @ zf_to_bool
  | Lst [Sym "num?"; arg] ->
    compile_exp arg @ [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ] @ zf_to_bool
  | _ -> failwith "I can't handle that sexp"

let compile (program: s_exp) : directive list =
  let directives = compile_exp program in
    [ Global "entry"; Label "entry"; ] @ directives @ [Ret ]