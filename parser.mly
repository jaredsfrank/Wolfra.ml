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
%token TAN
%token MULT
%token COS
%token PI
%token SEMI
%token COMMA
%token E
%token DECIMAL
%token SIN
%token SUBST
%token FOR
%token IN
%token LOG
%token DIV
%token PLUS
%token MINUS
%token DERIV
%token DERIVE
%token DERIV2
%token DERIVE2
%token INTEGRATE
%token <string> VAR
%token <string> FLOAT
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token TRANS
%token DET
%token EV
%token EVAL
%token RREF
%token INV
%token EOF

%left SUBST FOR IN
%left PLUS MINUS
%left MULT DIV COS SIN TAN LOG DERIV DERIVE DERIV2 DERIVE2
%left TRANS DET INV EV EVAL RREF
%right POW DECIMAL
%nonassoc VAR LBRACE FLOAT LPAREN PI E  COMMA SEMI LBRACKET




/* entry point */

%start expr
%type <Ast.expr> expr

%start m_list
%type <Ast.expr list> m_list

%start matrix
%type <Ast.expr list list> matrix


%%

expr:
  | FLOAT DECIMAL FLOAT {Float (float_of_string ($1^"."^$3))}
  | FLOAT    {Float (float_of_string $1)}
  | LPAREN expr RPAREN  %prec LPAREN
           { $2 }
  | expr POW expr    { BinOp (Pow, $1, $3) }
  | FLOAT expr       {BinOp (Times, Float (float_of_string $1), $2)}
  | expr MULT expr      { BinOp (Times,  $1, $3) }
  | MINUS expr         { UnOp (Neg, $2) }
  | SIN LPAREN expr RPAREN  {UnOp(Sin, $3)}
  | SUBST expr FOR expr IN expr {Subst ($2, $4, $6)}
  | COS LPAREN expr RPAREN  {UnOp(Cos, $3)}
  | LOG LPAREN expr RPAREN  {UnOp(Log, $3)}
  | expr DIV expr      { BinOp (Divide,  $1, $3) }
  | expr PLUS expr   { BinOp (Plus,   $1, $3) }
  | expr MINUS expr     { BinOp (Minus,  $1, $3) }
  | DERIVE expr DERIV expr     { BinOp  (Deriv, $2, $4) }
  | DERIVE2 expr DERIV2 expr     { BinOp  (Deriv, $2, $4) }
  | INTEGRATE expr DERIV expr     { BinOp  (Integrate, $2, $4) }
  | VAR    { Var $1 }
  | PI      { PI }
  | E      { E }
  | LBRACKET matrix RBRACKET {Matrix $2}
  | TRANS expr {UnOp (Trans, $2)}
  | DET expr {UnOp (Det, $2)}
  | INV expr {UnOp (Inv, $2)}
  | EV expr {UnOp (EigVector, $2)}
  | EVAL expr {UnOp (EigValue, $2)}
  | RREF expr {UnOp (RRef, $2)}
  | TAN LPAREN expr RPAREN  {UnOp(Tan, $3)}
;



matrix:
  | {[[]]}
  | m_list {[$1]}
  | matrix_inside {$1}
;

matrix_inside:
  | m_list {[$1]}
  | m_list SEMI matrix_inside {$1::$3}
;


m_list:
  | expr {[$1]}
  | expr COMMA m_list {$1::$3}
;

