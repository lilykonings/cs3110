%{
open Ast
open Identifier
open Lexing
%}

%token LPAREN
%token RPAREN
%token <int> NUMBER
%token <bool> BOOLEAN
%token <string> SYMBOL
%token DOT
%token QUOTE
%token EOF

%start main
%type <Ast.datum list> main
%type <Ast.datum> datum

%%

main:
  | datum main { $1 :: $2 }
  | EOF { [] }
;

datum:
  | literal { Atom ($1) }
  | symbol  { Atom ($1) }
  | cons { $1 }
  | nil { $1 }
  | quoted { $1 }
;

literal:
  | NUMBER  { Integer $1 }
  | BOOLEAN { Boolean $1 }

nil:
  | LPAREN RPAREN { Nil }
;

symbol:
  | SYMBOL { Identifier (identifier_of_string $1) }
;

cons:
  | LPAREN datum DOT datum RPAREN { Cons ($2, $4) }
  | LPAREN datum cons_tail { Cons ($2, $3) }
;

cons_tail:
  | RPAREN { Nil }
  | datum cons_tail { Cons ($1, $2) }
;

quoted:
  | QUOTE datum { Cons (Atom (Identifier (identifier_of_string "quote")),
                        Cons ($2, Nil)) }
;
