%{ open Exp %}

%token <int> NUMBER
%token <string> SYMBOL
%token <char> CHARACTER
%token <string> STRING
%token LPAREN RPAREN
%token DOTS
%token EOF

%type <Exp.t list> many
%type <Exp.t> main
%start main many

%%

main:
| e = expr EOF
        { e }

many:
| EOF
  { [] }
| e = expr l = many
  { e :: l }

expr:
| n = NUMBER
  { Num n }
| c = CHARACTER
  { Chr c }
| s = STRING
  { Str s }
| s = SYMBOL
  { Sym s }
| DOTS
  { Dots }
| LPAREN l=lst RPAREN
  { Lst l }
lst:
|   { [] }
| e = expr l = lst
    { e ::l }
