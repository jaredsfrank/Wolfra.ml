%{
(* This parser is implemented with ocamlyacc, not menhir. *)

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


%token PLUS
%token MINUS
%token MULT
%token POW
%token <string> VAR
%token <string> STRING
%token <string> INT
%token LPAREN RPAREN
%token EOF

%right POW
%left PLUS MINUS
%left MULT
%nonassoc VAR LBRACE INT LPAREN


/* entry point */

%start expr
%type <Ast.expr> expr


%%

expr:
  | INT    {Float (float_of_string $1)}
  | expr PLUS expr      { BinOp (Plus,   $1, $3) }
  | expr MINUS expr     { BinOp (Minus,  $1, $3) }
  | expr MULT expr      { BinOp (Times,  $1, $3) }
  | expr POW expr    { BinOp (Pow, $1, $3) }
  | VAR    { Var $1 }
  | LPAREN expr RPAREN  %prec LPAREN
           { $2 }
;




