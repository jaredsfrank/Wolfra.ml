open Ast
open Printer
open Eval
open Parse

let rec main () = 
    Printf.printf "Write an expression: \n";
    let result = String.lowercase(read_line ()) in
    try
        match result with
        | "quit" -> Printf.printf "done"; ()
        | s -> Printf.printf "Result: %s\n\n" (string_of_expr (eval (parse_expr s))); main()
    with
    | Failure "TODO" -> Printf.printf "That feature is not yet implemented\n\n"; main()
    | Failure "Division by 0" -> Printf.printf "Uh oh...Division by 0\n\n"; main()
    | Failure "This shouldn't happen" -> Printf.printf "Please derive with respect to a variable\n\n"; main()
    | _  -> Printf.printf "Please enter in a proper expression\n\n"; main()

let () = main ()