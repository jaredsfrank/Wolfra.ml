open Printer
open Eval
open Parse



let rec main () = 
    Printf.printf "\n>> ";
    let result = read_line () in
    try
        match result with
        | "quit" -> print "Done. Press enter to leave"; ()
        | "clear" -> env := []; print "";main()
        | "help" -> let _ = Sys.command "clear" in print_help (); print_main_help (); help_menu ()
        | s -> let e =  (eval (parse_expr s)) in
                print_expr e; 
                prev := e; main()
    with
    | Stack_overflow -> print "Wolfra.ml does not support that type of integration!"; main ()
    | Failure "Parse error: (1.0-1.1" -> print "Please enter in a proper expression"; main ()
    | Failure s -> print s; main ()
    | _  -> print "Error"; main()


and help_menu () =
    Printf.printf ">> ";
    let input = String.lowercase(read_line ()) in
    let _ = Sys.command "clear" in
    match input with
    | "main" ->  print_help (); print_main_help (); help_menu ();
    | "basic operation" -> print_basic_help ();  help_menu ()
    | "derivatives" -> print_deriv_help (); help_menu ()
    | "integrals" -> print_integ_help (); help_menu ()
    | "matrices" -> print_matr_help  (); help_menu ()
    | "" -> let _ = Sys.command "clear" in print_intro (); main()
    | _ -> print_help (); print_main_help ();help_menu ()



let () = let _ = Sys.command "clear" in print_intro (); main ()



