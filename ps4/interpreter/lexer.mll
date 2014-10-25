{
open Parser
exception UnexpectedCharacter of char

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    {
      pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

(* The following grammar uses names that are taken from R5RS directly.  It's a
   simplified subset of the language, so it doesn't have the full complexity of
   that spec. *)

(* Character classes *)
let letter = [ 'a'-'z' 'A'-'Z' ]
let digit = [ '0'-'9' ]
let whitespace = [ ' ' '\t' ] (* exclude \n here; we want to incr linenum *)
let newline = [ '\n' ]

(* Identifiers *)
let special_initial =
  [ '!' '$' '%' '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~' ]
let initial = letter | special_initial

let special_subsequent = [ '+' '-' '.' '@' ]
let subsequent = initial | digit | special_subsequent

let peculiar_identifier = [ '+' '-' ] | "..."
let identifier = peculiar_identifier | (initial subsequent* )

(* Integers.  We fudge the grammar from R5RS here because we don't need the full
   complexity of Scheme numbers. *)
let sign = [ '+' '-' ]

let uinteger_base10 = digit+
let num_base10 = sign? uinteger_base10

(* Literals *)
let boolean = "#t" | "#f"
let number = num_base10
let symbol = identifier

rule token = parse
   | "("          { LPAREN }
   | ")"          { RPAREN }
   | "."          { DOT }
   | "'"          { QUOTE }
(*   | "`"          { QQUOTE }
   | ","          { QQUOTE_COMMA }
   | ",@"         { QQUOTE_COMMA_AT }*)
   | boolean as b { BOOLEAN (b = "#t") }
   | number as n  { NUMBER (int_of_string n) }
   | symbol as id { SYMBOL (id) }
   | whitespace   { token lexbuf } (* skip blanks *)
   | newline      { incr_linenum lexbuf; token lexbuf }
   | eof          { EOF }
   | ";"          { comment lexbuf }
   | _ as c       { raise (UnexpectedCharacter c) }
  and comment = parse
   | eof { EOF }
   | newline { incr_linenum lexbuf; token lexbuf }
   | _ { comment lexbuf }
