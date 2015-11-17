open Parser
open Lexer

let parse_expr s =

    expr token (Lexing.from_string s)

