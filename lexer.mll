{
open Parser

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

}


let ident      = (['a'-'z'] | '_') (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')*
let constr     = ['A'-'Z'] (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')*
let integral   = ['0'-'9']+
let whitespace = [' ' '\t']

let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule token = parse
  | whitespace { token lexbuf } (* skip blanks *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "^" { POW }
  | "("    { LPAREN }
  | ")"    { RPAREN }
  | id as s    { VAR s }
  | integral as f  { FLOAT (float_of_string f)}