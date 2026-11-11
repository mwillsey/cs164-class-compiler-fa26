type token = PLUS | TIMES | LPAREN | RPAREN | NUM of int
type expr = Num of int | Plus of expr * expr | Times of expr * expr

(*
A parser for the grammar:

expr ::= expr + expr | expr * expr | (expr) | NUM

But that's ambiguous, so I factored it so we can implement
 a non-ambigous, recursive descent parser.

expr   ::= term expr'
expr'  ::= + expr | ε
term   ::= factor term'
term'  ::= * term | ε
factor ::= NUM | (expr)
*)

exception ParseError

let token_of_string (s : string) =
  match s with
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "+" -> PLUS
  | "*" -> TIMES
  | _ -> NUM (int_of_string s)

let tokenize (s : string) =
  s |> String.split_on_char ' ' |> List.map token_of_string

let rec parse_expr toks =
  let t, toks = parse_term toks in
  parse_expr' t toks

and parse_expr' t toks =
  match toks with
  | PLUS :: toks ->
      let e, toks = parse_expr toks in
      (Plus (t, e), toks)
  | _ -> (t, toks)

and parse_term toks =
  let f, toks = parse_factor toks in
  parse_term' f toks

and parse_term' f toks =
  match toks with
  | TIMES :: toks ->
      let e, toks = parse_term toks in
      (Times (f, e), toks)
  | _ -> (f, toks)

and parse_factor toks =
  match toks with
  | NUM n :: toks -> (Num n, toks)
  | LPAREN :: toks -> (
      let e, toks = parse_expr toks in
      match toks with RPAREN :: toks -> (e, toks) | _ -> raise ParseError)
  | _ -> raise ParseError

let parse (s : string) =
  let toks = tokenize s in
  let exp, l = parse_expr toks in
  if List.length l = 0 then exp else raise ParseError
