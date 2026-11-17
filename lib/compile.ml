open Ast
open Shared.Directive
open Shared.Error

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00
let bool_shift = 7
let bool_mask = 0b1111111
let bool_tag = 0b0011111
let pair_tag = 0b010
let heap_mask = 0b111

let function_label s =
  let nasm_char c =
    match c with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' | '$' | '#' | '@' | '~' | '.' | '?' ->
        c
    | _ -> '_'
  in
  Printf.sprintf "function_%s_%d" (String.map nasm_char s) (Hashtbl.hash s)

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
let compile_prim0 stack_index = function
  | ReadNum ->
      [
        Mov (stack_addr stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "read_num";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_addr stack_index);
      ]
  | Newline ->
      [
        Mov (stack_addr stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "print_newline";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_addr stack_index);
        Mov (Reg Rax, operand_of_bool true);
      ]

let compile_prim1 stack_index = function
  | Add1 -> ensure_num (Reg Rax) @ [ Add (Reg Rax, operand_of_num 1) ]
  | Sub1 -> ensure_num (Reg Rax) @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Not -> [ Cmp (Reg Rax, operand_of_bool false) ] @ zf_to_bool
  | IsZero -> [ Cmp (Reg Rax, operand_of_num 0) ] @ zf_to_bool
  | IsNum ->
      [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ] @ zf_to_bool
  | Left ->
      ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag))) ]
  | Right ->
      ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8))) ]
  | Print ->
      [
        Mov (stack_addr stack_index, Reg Rdi);
        Mov (Reg Rdi, Reg Rax);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "print_value";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_addr stack_index);
        Mov (Reg Rax, operand_of_bool true);
      ]
  | IsPair -> failwith "unexpected prim pair?"
  | IsEmpty -> failwith "unexpected prim empty?"

let compile_prim2 stack_index = function
  | Plus ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_addr stack_index) ]
      @ [ Add (Reg Rax, Reg R8) ]
  | Minus ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, Reg Rax) ]
      @ [ Mov (Reg Rax, stack_addr stack_index) ]
      @ [ Sub (Reg Rax, Reg R8) ]
  | Eq ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_addr stack_index) ]
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | Lt ->
      ensure_num (stack_addr stack_index)
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, Reg Rax) ]
      @ [ Mov (Reg Rax, stack_addr stack_index) ]
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ lf_to_bool
  | Pair ->
      [
        Mov (Reg R8, stack_addr stack_index);
        Mov (MemOffset (Reg Rdi, Imm 0), Reg R8);
        Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax);
        Mov (Reg Rax, Reg Rdi);
        Or (Reg Rax, Imm pair_tag);
        Add (Reg Rdi, Imm 16);
      ]

let rec compile_exp defns (env : int Symtab.t) (stack_index : int) (exp : expr)
    (is_tail : bool) : directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | True -> [ Mov (Reg Rax, operand_of_bool true) ]
  | False -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Call (f, args) when is_defn defns f ->
      let defn = get_defn defns f in
      if List.length args = List.length defn.args then
        if is_tail then
          let compiled_args =
            args
            |> List.mapi (fun i arg ->
                compile_exp defns env (stack_index - (8 * i)) arg false
                @ [ Mov (stack_addr (stack_index - (8 * i)), Reg Rax) ])
            |> List.concat
          in
          let moved_args =
            args
            |> List.mapi (fun i _ ->
                [
                  Mov (Reg R8, stack_addr (stack_index - (8 * i)));
                  Mov (stack_addr ((i + 1) * -8), Reg R8);
                ])
            |> List.concat
          in
          compiled_args @ moved_args @ [ Jmp (function_label f) ]
        else
          let stack_base = align_stack_index (stack_index + 8) in
          let compiled_args =
            args
            |> List.mapi (fun i arg ->
                compile_exp defns env (stack_base - (8 * (i + 2))) arg false
                @ [ Mov (stack_addr (stack_base - (8 * (i + 2))), Reg Rax) ])
            |> List.concat
          in
          compiled_args
          @ [
              Add (Reg Rsp, Imm stack_base);
              Call (function_label f);
              Sub (Reg Rsp, Imm stack_base);
            ]
      else failwith "wrong num of args"
  | Do exps when List.length exps > 0 ->
      List.mapi
        (fun i exp ->
          compile_exp defns env stack_index exp
            (if i = List.length exps - 1 then is_tail else false))
        exps
      |> List.concat
  | If (e_cond, e_then, e_else) ->
      let label_else = gensym "else" in
      let label_then = gensym "then" in
      let label_done = gensym "done" in
      compile_exp defns env stack_index e_cond false
      @ [ Cmp (Reg Rax, operand_of_bool false); Je label_else ]
      @ [ Label label_then ]
      @ compile_exp defns env stack_index e_then is_tail
      @ [ Jmp label_done ] @ [ Label label_else ]
      @ compile_exp defns env stack_index e_else is_tail
      @ [ Label label_done ]
  | Let (var, e, body) ->
      compile_exp defns env stack_index e false
      @ [ Mov (stack_addr stack_index, Reg Rax) ]
      @ compile_exp defns
          (Symtab.add var stack_index env)
          (stack_index - 8) body is_tail
  | Var var -> [ Mov (Reg Rax, stack_addr (Symtab.find var env)) ]
  | Prim2 (prim, a, b) ->
      compile_exp defns env stack_index a false
      @ [ Mov (stack_addr stack_index, Reg Rax) ]
      @ compile_exp defns env (stack_index - 8) b false
      @ compile_prim2 stack_index prim
  | Prim1 (prim, arg) ->
      compile_exp defns env stack_index arg false
      @ compile_prim1 stack_index prim
  | Prim0 prim -> compile_prim0 stack_index prim
  | _ -> raise (Stuck (s_exp_of_expr exp))

let compile_defn defns defn : directive list =
  let ftab =
    defn.args
    |> List.mapi (fun i arg -> (arg, -8 * (i + 1)))
    |> List.to_seq |> Symtab.of_seq
  in
  [ Label (function_label defn.name) ]
  @ compile_exp defns ftab (-8 * (List.length defn.args + 1)) defn.body true
  @ [ Ret ]

let compile (program : program) : directive list =
  let defns = program.defns in
  let directives = compile_exp defns Symtab.empty (-8) program.body true in
  [
    Global "entry";
    Extern "error";
    Extern "read_num";
    Extern "print_newline";
    Extern "print_value";
    Label "entry";
  ]
  @ directives @ [ Ret ]
  @ List.concat_map (compile_defn defns) defns
