open Eval
let rec step_string _ = failwith "TODO"
(*This code is inspired by the printer from A4 OCalf*)

let string_of_floats f =
    let s = string_of_float f in
    if String.get s ((String.length s)-1) = '.' then String.sub s 0 ((String.length s)-1)
    else s

let rec format_expr f e = 
    let bracket parent f e =
        Format.fprintf f "%a" format_expr e
    in
    let rec print_list f = function
        | [] -> Format.fprintf f ""
        | [h] -> Format.fprintf f "%a" (bracket e) h
        | h::t -> Format.fprintf f "%a,%a" (bracket e) h (print_list) t
    in
    let rec print_list_list f = function
        | [] -> Format.fprintf f ""
        | [h] -> Format.fprintf f "%a" print_list h
        | h::t -> Format.fprintf f "%a;%a" print_list h (print_list_list) t
    in
    match e with
    | SFloat n -> Format.fprintf f "%s" (string_of_floats n)
    | SVar x -> Format.fprintf f "%s" x
    | STimes (c,[]) -> Format.fprintf f "%s" (string_of_floats c)
    | STimes (1.,[h]) -> Format.fprintf f "%a" (bracket e) h
    | STimes (-1.,[h]) -> Format.fprintf f "-%a" (bracket e) h
    | STimes (c,[h]) -> Format.fprintf f "%s%a" (string_of_floats c) (bracket e) h
    | STimes (1., h::t) -> Format.fprintf f "%a*%a" (bracket e) h (bracket e) (STimes (1.,t))
    | STimes (c, h::t) -> Format.fprintf f "%s%a*%a" (string_of_floats c) (bracket e) h (bracket e) (STimes (1.,t))
    | SPlus [] -> Format.fprintf f ""
    | SPlus [h] -> Format.fprintf f "%a" (bracket e) h
    | SPlus (h1::h2::t) -> (match h2 with 
                              | SFloat x when x<0. -> Format.fprintf f "%a-%a" (bracket e) h1 (bracket e) (SPlus ((SFloat (-1.*.x))::t))
                              | STimes (c,x) when c<0. -> Format.fprintf f "%a-%a" (bracket e) h1 (bracket e) (SPlus (STimes (-1.*.c,x)::t))
                              | _ -> Format.fprintf f "@[<hov 1>%a@,+%a@]" (bracket e) h1 (bracket e) (SPlus( h2::t)))
    | SPow (s1,s2) -> Format.fprintf f "@[(%a)^(%a)@]" (bracket e) s1 (bracket e) s2
    | SMatrix []  -> Format.fprintf f ""
    | SMatrix [h] -> Format.fprintf f "[%a]" print_list h
    | SMatrix (h::t) -> Format.fprintf f "[%a;%a]" print_list h print_list_list t
    | SSin s ->  Format.fprintf f "sin(%a)" (bracket e) s
    | SCos s ->  Format.fprintf f "cos(%a)" (bracket e) s
    | STan s ->  Format.fprintf f "tan(%a)" (bracket e) s
    | SLog s ->  Format.fprintf f "ln(%a)" (bracket e) s
    | SE -> Format.fprintf f "e"
    | SPI  -> Format.fprintf f "pi"
let make_string_of f = Format.asprintf "%a" f
let string_of_expr     = make_string_of format_expr
