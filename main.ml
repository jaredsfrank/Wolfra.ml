open Printer
open Eval
open Parse



let rec main () = 
    Printf.printf "\n>> ";
    let result = read_line () in
    try
        match result with
        | "quit" -> print "done"; ()
        | "clear" -> env := []; print "";main()
        | s -> let (e,l) =  (eval (parse_expr s)) in
                print_expr e; 
                List.iter print l;
                prev := e; main()
    with
    | Failure "TODO" -> print "That feature is not yet implemented"; main()
    | Failure "Division by 0" -> print "Uh oh...Division by 0"; main()
    | Failure "This shouldn't happen" -> print "Please derive with respect to a variable"; main()
    | Stack_overflow -> print "Wolfra.ml does not support that type of integration!"; main ()
    | Failure "Err Matrix Mult" -> print "These matrices do not multiply"; main ()
    | Failure "Err Square" -> print "The matrix must be square"; main ()
    | Failure "No complex solutions" -> print "Complex solutions are not supported"; main ()
    | _  -> print "Please enter in a proper expression"; main()

let () = let _ = Sys.command "clear" in print_intro (); main ()



