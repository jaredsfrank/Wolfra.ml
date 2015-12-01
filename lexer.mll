{
open Parser
exception Eof


(* This code is heavily inspired by the Lexer for OCalf, A4*)

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let letters     = (['a'-'z'] | ['A'-'Z'])*
let integral   = ['0'-'9']+
let whitespace = [' ' '\t']

rule token = parse
  | whitespace { token lexbuf } (* skip blanks *)
  | ['\n'] { incr_linenum lexbuf; token lexbuf }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "." { DECIMAL }
  | "^" { POW }
  | "/" { DIV }
  | "cos" { COS }
  | "sin" { SIN }
  | "log" { LOG }
  | "derive" { DERIVE }
  | "d(" { DERIVE2 }
  | "pi" { PI }
  | "wrt" {DERIV}
  | ")/d"  { DERIV2 }
  | "integrate" { INTEGRATE }
  | "e" { E }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | "sub" {SUBST}
  | "for"      {FOR}
  | ","        {COMMA}
  | ";"        {SEMI}
  | "in"       {IN}
  | "["    { LBRACKET }
  | "]"    { RBRACKET }
  | "transpose"  {TRANS}
  | "det"      { DET }
  | "inverse"     { INV }
  | "eigenvector" {EV}
  | "eigenvalue" {EVAL}
  | "rref" {RREF}
  | letters as id { VAR id }
  | integral as i {FLOAT i}
  | eof { EOF }
  | "tan" { TAN } 

