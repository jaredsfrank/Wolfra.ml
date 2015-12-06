open Simplify


(*[string_of_expr s] returns a string representation of s*)
val string_of_expr     : s_expr     -> string

(*[print_expr s] prints a string representation of s*)
val print_expr : s_expr -> unit

(*[print s] s to the terminal in a formatted way*)
val print: string -> unit

(*Prints the intro message*)
val print_intro: unit -> unit

(*Prints the help logo*)
val print_help: unit -> unit

(*Prints the main help menu*)
val print_main_help:  unit -> unit

(*Prints the basic operations help menu*)
val print_basic_help: unit -> unit

(*Prints the derivative help menu*)
val print_deriv_help: unit -> unit 

(*Prints the integral help menu*)
val print_integ_help: unit -> unit

(*Prints the matrix help menu*)
val print_matr_help: unit -> unit

