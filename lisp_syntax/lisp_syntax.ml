open S_exp
open Ast

exception ParseError of s_exp

let prim0_of_string : string -> prim0 option =
 fun s ->
  begin match s with
  | "read-num" -> Some ReadNum
  | "newline" -> Some Newline
  | _ -> None
  end

let prim1_of_string : string -> prim1 option =
 fun s ->
  begin match s with
  | "add1" -> Some Add1
  | "sub1" -> Some Sub1
  | "zero?" -> Some IsZero
  | "num?" -> Some IsNum
  | "pair?" -> Some IsPair
  | "empty?" -> Some IsEmpty
  | "not" -> Some Not
  | "left" -> Some Left
  | "right" -> Some Right
  | "print" -> Some Print
  | _ -> None
  end

let prim2_of_string : string -> prim2 option =
 fun s ->
  begin match s with
  | "+" -> Some Plus
  | "-" -> Some Minus
  | "=" -> Some Eq
  | "<" -> Some Lt
  | "pair" -> Some Pair
  | _ -> None
  end

let rec expr_of_s_exp : s_exp -> expr =
 fun s_exp ->
  begin match s_exp with
  | Num x -> Num x
  | Sym "true" -> True
  | Sym "false" -> False
  | Sym var -> Var var
  | Lst [] -> Nil
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; exp ] ]; body ] ->
      Let (var, expr_of_s_exp exp, expr_of_s_exp body)
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      Do (List.map expr_of_s_exp exps)
  | Lst [ Sym "if"; test_s; then_s; else_s ] ->
      If (expr_of_s_exp test_s, expr_of_s_exp then_s, expr_of_s_exp else_s)
  | Lst [ Sym prim ] when Option.is_some (prim0_of_string prim) ->
      Prim0 (Option.get (prim0_of_string prim))
  | Lst [ Sym prim; arg ] when Option.is_some (prim1_of_string prim) ->
      Prim1 (Option.get (prim1_of_string prim), expr_of_s_exp arg)
  | Lst [ Sym prim; arg1; arg2 ] when Option.is_some (prim2_of_string prim) ->
      Prim2
        ( Option.get (prim2_of_string prim),
          expr_of_s_exp arg1,
          expr_of_s_exp arg2 )
  | Lst (Sym f :: args) -> Call (f, List.map expr_of_s_exp args)
  | e -> raise (ParseError e)
  end

let program_of_s_exps : s_exp list -> program =
  let rec get_args : s_exp list -> string list =
   fun args ->
    begin match args with
    | Sym v :: args -> v :: get_args args
    | e :: _ -> raise (ParseError e)
    | [] -> []
    end
  in
  let get_defn : s_exp -> defn =
   fun s_exp ->
    begin match s_exp with
    | Lst [ Sym "define"; Lst (Sym name :: args); body ] ->
        { name; args = get_args args; body = expr_of_s_exp body }
    | e -> raise (ParseError e)
    end
  in
  let rec go : defn list -> s_exp list -> program =
   fun defns s_exps ->
    begin match s_exps with
    | [ e ] -> { defns = List.rev defns; body = expr_of_s_exp e }
    | d :: rest -> go (get_defn d :: defns) rest
    | _ -> raise (ParseError (Sym "empty"))
    end
  in
  go []

let parse : string -> program =
 fun s -> s |> S_exp.parse_many |> program_of_s_exps
