%{
(* This parser is implemented with ocamlyacc, not menhir. *)
(* This code is heavily inspired by the parser for OCalf, A4*)

open Ast
open Lexing

let parse_error _ =
  let start_pos = Parsing.symbol_start_pos () in
  let end_pos = Parsing.symbol_end_pos () in
  let start_line = string_of_int start_pos.pos_lnum in
  let start_char = string_of_int (start_pos.pos_cnum - start_pos.pos_bol) in
  let end_line = string_of_int end_pos.pos_lnum in
  let end_char = string_of_int (end_pos.pos_cnum - end_pos.pos_bol) in
  failwith ("Parse error: ("^start_line^"."^start_char^"-"^end_line^"."^end_char)
%}


%token POW
%token MULT
%token COS
%token PI
%token E
%token SIN
%token LOG
%token DIV
%token PLUS
%token MINUS
%token DERIV
%token DERIVE
%token DERIV2
%token DERIVE2
%token <string> VAR
%token <string> FLOAT
%token LPAREN RPAREN
%token EOF


%left PLUS MINUS
%left MULT DIV COS SIN LOG DERIV DERIVE DERIV2 DERIVE2
%right POW
%nonassoc VAR LBRACE FLOAT LPAREN PI E


/* entry point */

%start expr
%type <Ast.expr> expr


%%

expr:
  | FLOAT    {Float (float_of_string $1)}
  | LPAREN expr RPAREN  %prec LPAREN
           { $2 }
  | expr POW expr    { BinOp (Pow, $1, $3) }
  | expr MULT expr      { BinOp (Times,  $1, $3) }
  | MINUS expr         { UnOp (Neg, $2) }
  | SIN LPAREN expr RPAREN  {UnOp(Sin, $3)}
  | COS LPAREN expr RPAREN  {UnOp(Cos, $3)}
  | LOG LPAREN expr RPAREN  {UnOp(Log, $3)}
  | expr DIV expr      { BinOp (Divide,  $1, $3) }
  | expr PLUS expr   { BinOp (Plus,   $1, $3) }
  | expr MINUS expr     { BinOp (Minus,  $1, $3) }
  | DERIVE expr DERIV expr     { BinOp  (Deriv, $2, $4) }
  | DERIVE2 expr DERIV2 expr     { BinOp  (Deriv, $2, $4) }
  | VAR    { Var $1 }
  | PI      { PI }
  | E      { E }
;




