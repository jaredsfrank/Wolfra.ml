open Simplify

type deriv_print = 
| PNone
| PConst of s_expr * s_expr
| PVar of s_expr * s_expr
| PProd of s_expr * s_expr
| PPlus of s_expr * s_expr
| PPow of s_expr * s_expr 
| PSin of s_expr * s_expr 
| PCos of s_expr * s_expr
| PLog of s_expr * s_expr

(*[string_of_expr s] returns a string representation of s*)
val string_of_expr     : s_expr     -> string

val print_expr : s_expr -> unit

val print: string -> unit

val print_intro: unit -> unit
