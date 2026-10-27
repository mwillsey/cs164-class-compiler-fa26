type s_exp = Num of int | Sym of string | Lst of s_exp list
type token = NUM of int | SYM of string | LPAREN | RPAREN

exception ParseError

(* very bad tokenizer! *)
let token_of_string (s : string) =
  match s with
  | "(" -> LPAREN
  | ")" -> RPAREN
  | _ -> ( try NUM (int_of_string s) with _ -> SYM s)

let tokenize (s : string) =
  s |> String.split_on_char ' ' |> List.map token_of_string
(* end bad tokenizer *)

let rec parse_s_exp (toks : token list) : s_exp * token list =
  match toks with
  | NUM n :: toks -> (Num n, toks)
  | SYM s :: toks -> (Sym s, toks)
  | LPAREN :: toks ->
      let exps, toks = parse_lst toks in
      (Lst exps, toks)
  | _ -> raise ParseError

and parse_lst (toks : token list) : s_exp list * token list =
  match toks with
  | RPAREN :: toks -> ([], toks)
  | _ ->
      let exp, toks = parse_s_exp toks in
      let exps, toks = parse_lst toks in
      (exp :: exps, toks)
