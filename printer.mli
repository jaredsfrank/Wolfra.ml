open Simplify
(*Returns the string interpretation of performing one arithmetic step*)
val step_string : s_expr -> string

(*Returns the string form of an expr AST for printing the result*)
val format_expr : Format.formatter -> s_expr     -> unit


val make_string_of : (Format.formatter -> 'a -> unit) -> 'a -> string

val string_of_expr     : s_expr     -> string