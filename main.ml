open Printer
open Eval
open Parse

let print_intro () = 
    Printf.printf "__        __    _  __                       _ 
\\ \\      / /__ | |/ _|_ __ __ _   _ __ ___ | |
 \\ \\ /\\ / / _ \\| | |_| '__/ _` | | '_ ` _ \\| |
  \\ V  V / (_) | |  _| | | (_| |_| | | | | | |
   \\_/\\_/ \\___/|_|_| |_|  \\__,_(_)_| |_| |_|_|\n\n\n";
    Printf.printf ("Welcome to Wolfra.ml!\nType help at any time for the help function. Type quit to exit the program.\n\nCopyright (c) 2015 Wolfra.ml Industries\nAll Rights Reserved\n\nThis product is protected by copyright and distributed under\nlicenses restricting copying, distribution and decompilation.\n\n"); ()

let rec main () = 
    Printf.printf "Write an expression: \n>> ";
    let result = read_line () in
    try
        match result with
        | "quit" -> Printf.printf "done"; ()
        | "clear" -> env := []; Printf.printf "\n\n\n";main()
        | s -> Printf.printf "-----------------------------------------------\n"; print_expr (eval (parse_expr s)); Printf.printf "\n\n\n"; 
               prev := (eval (parse_expr s)); main()
    with
    | Failure "TODO" -> Printf.printf "That feature is not yet implemented\n\n"; main()
    | Failure "Division by 0" -> Printf.printf "Uh oh...Division by 0\n\n"; main()
    | Failure "This shouldn't happen" -> Printf.printf "Please derive with respect to a variable\n\n"; main()
    | Stack_overflow -> Printf.printf "Wolfra.ml does not support that type of integration!\n\n"; main ()
    | Failure "Err Matrix Mult" -> Printf.printf "These matrices do not multiply\n\n"; main ()
    | Failure "Err Square" -> Printf.printf "The matrix must be square\n\n"; main ()
    | Failure "No complex solutions" -> Printf.printf "Complex solutions are not supported\n\n"; main ()
    | _  -> Printf.printf "Please enter in a proper expression\n\n"; main()

let () = let _ = Sys.command "clear" in print_intro (); main ()



