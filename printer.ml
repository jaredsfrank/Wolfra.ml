open Simplify
(*This code is inspired by the printer from A4 OCalf*)

let string_of_floats f =
    let s = string_of_float f in
    if String.get s ((String.length s)-1) = '.' then String.sub s 0 ((String.length s)-1)
    else s

let rec format_expr f e = 
    let bracket parent f e =
        Format.fprintf f "@{<misc>%a@}" format_expr e
    in
    let rec print_list f = function
        | [] -> Format.fprintf f "@{<misc>"
        | [h] -> Format.fprintf f "@{<misc>%a@}" (bracket e) h
        | h::t -> Format.fprintf f "@{<misc>%a,%a@}" (bracket e) h (print_list) t
    in
    let rec print_list_list f = function
        | [] -> Format.fprintf f "@{<misc>"
        | [h] -> Format.fprintf f "@{<misc>%a@}" print_list h
        | h::t -> Format.fprintf f "@{<misc>%a;%a@}" print_list h (print_list_list) t
    in
    match e with
    | SFloat n -> Format.fprintf f "@{<misc>%s@}" (string_of_floats n)
    | SVar x -> Format.fprintf f "@{<var>%s@}" x
    | STimes (c,[]) -> Format.fprintf f "@{<misc>%s@}" (string_of_floats c)
    | STimes (1.,[h]) -> Format.fprintf f "@{<misc>%a@}" (bracket e) h
    | STimes (-1.,[h]) -> Format.fprintf f "@{<misc>-%a@}" (bracket e) h
    | STimes (c,[h]) -> Format.fprintf f "@{<misc>%s%a@}" (string_of_floats c) (bracket e) h
    | STimes (1., h::t) -> Format.fprintf f "@{<misc>%a*%a@}" (bracket e) h (bracket e) (STimes (1.,t))
    | STimes (c, h::t) -> Format.fprintf f "@{<misc>%s%a*%a@}" (string_of_floats c) (bracket e) h (bracket e) (STimes (1.,t))
    | SPlus [] -> Format.fprintf f "@{<misc>"
    | SPlus [h] -> Format.fprintf f "@{<misc>%a@}" (bracket e) h
    | SPlus (h1::h2::t) -> (match h2 with 
                              | SFloat x when x<0. -> Format.fprintf f "@{<misc>%a-%a@}" (bracket e) h1 (bracket e) (SPlus ((SFloat (-1.*.x))::t))
                              | STimes (c,x) when c<0. -> Format.fprintf f "@{<misc>%a-%a@}" (bracket e) h1 (bracket e) (SPlus (STimes (-1.*.c,x)::t))
                              | _ -> Format.fprintf f "@{<misc>%a+%a@}" (bracket e) (SPlus( h2::t)) (bracket e) h1)
    | SPow (s1,s2) -> Format.fprintf f "@{<misc>(%a)^(%a)@}" (bracket e) s1 (bracket e) s2
    | SMatrix []  -> Format.fprintf f "@{<misc>"
    | SMatrix [h] -> Format.fprintf f "@{<misc>[%a]@}" print_list h
    | SMatrix (h::t) -> Format.fprintf f "@{<misc>[%a;%a]@}" print_list h print_list_list t
    | SSin s ->  Format.fprintf f "@{<misc>sin(%a)@}" (bracket e) s
    | SCos s ->  Format.fprintf f "@{<misc>cos(%a)@}" (bracket e) s
    | SLog s ->  Format.fprintf f "@{<misc>ln(%a)@}" (bracket e) s
    | SE -> Format.fprintf f "@{<misc>e@}"
    | SPI  -> Format.fprintf f "@{<misc>pi@}"



let clear_color _ = "\027[38;5;5m\027[0m"

let set_color = function
  | "misc"  -> "\027[38;5;15m"  (* blue   *)
  |  "var"  -> "\027[38;5;15m" (* red    *)
  |   _     -> "\027[38;5;5m\027[0m"


let print_tags = {
  Format.mark_open_tag   = set_color;
  Format.mark_close_tag  = clear_color;
  Format.print_open_tag  = ignore;
  Format.print_close_tag = ignore;
}

let make_printer formatter e =
  Format.set_margin 80;
  Format.set_formatter_tag_functions print_tags;
  Format.set_tags true;
  Format.printf "@<0>%s" (clear_color ());
  Format.printf "%a@." formatter e

let print_expr     = make_printer format_expr

let make_string_of f = Format.asprintf "%a" f
let string_of_expr     = make_string_of format_expr
