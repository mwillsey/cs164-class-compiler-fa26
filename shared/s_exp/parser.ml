open Stdlib

let parse (s : string) =
  let buf = Lexing.from_string s in
  Parse.main Lex.token buf

let parse_many (s : string) =
  let buf = Lexing.from_string s in
  Parse.many Lex.token buf

let parse_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parse.main Lex.token lexbuf in
  close_in inx ; ast

let parse_file_many file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parse.many Lex.token lexbuf in
  close_in inx ; ast
