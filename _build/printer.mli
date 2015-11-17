open Ast
(*Returns the string interpretation of performing one arithmetic step*)
val step_string : expr -> string

(*Returns the string form of an expr AST for printing the result*)
val to_string : expr -> string
