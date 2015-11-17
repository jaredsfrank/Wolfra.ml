type token =
  | PLUS
  | MINUS
  | MULT
  | POW
  | VAR of (string)
  | FLOAT of (float)
  | LPAREN
  | RPAREN

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
