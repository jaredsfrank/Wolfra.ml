open Simplify
open Fractions
(*This code is heavily inspired by the printer from A4 OCalf*)

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
    let print_frac f d = 
      match is_int d with
      | true -> Format.fprintf f "%s" (string_of_floats d)
      | false -> let (num, den) = frac d in
                if is_int ((float_of_int den)/.10000.) then Format.fprintf f "%s" (string_of_floats d) else
                 Format.fprintf f "(%s/%s)" (string_of_int num) (string_of_int den)
    in
    match e with
    | SFloat n -> print_frac f n
    | SVar x -> Format.fprintf f "%s" x
    | STimes (c,[]) -> Format.fprintf f "%a" (print_frac) c
    | STimes (1.,[h]) -> Format.fprintf f "%a" (bracket e) h
    | STimes (-1.,[h]) -> Format.fprintf f "-%a" (bracket e) h
    | STimes (c,[SPow(x, SFloat (-1.))]) -> Format.fprintf f "%a/%a" (print_frac) c (bracket e) x
    | STimes (c,[h]) -> Format.fprintf f "%a%a" (print_frac) c (bracket e) h
    | STimes (1., h::(SPow (e, SFloat (-1.)))::t) -> Format.fprintf f "%a/(%a)" (bracket e) h (bracket e) (STimes (1.,e::t))
    | STimes (c, h::(SPow (e, SFloat (-1.)))::t) -> Format.fprintf f "%a%a/(%a)" (print_frac) c (bracket e) h (bracket e) (STimes (1.,e::t))
    | STimes (1., h::t) -> Format.fprintf f "%a*%a" (bracket e) h (bracket e) (STimes (1.,t))
    | STimes (c, h::t) -> Format.fprintf f "%a%a*%a" (print_frac) c (bracket e) h (bracket e) (STimes (1.,t))
    | SPlus [] -> Format.fprintf f ""
    | SPlus [h] -> Format.fprintf f "%a" (bracket e) h
    | SPlus (h1::h2::t) -> (match h2 with 
                              | SFloat x when x<0. -> Format.fprintf f "%a-%a" (bracket e) h1 (bracket e) (SPlus ((SFloat (-1.*.x))::t))
                              | STimes (c,x) when c<0. -> Format.fprintf f "%a-%a" (bracket e) h1 (bracket e) (SPlus (STimes (-1.*.c,x)::t))
                              | _ -> Format.fprintf f "%a+%a" (bracket e) h1 (bracket e) (SPlus( h2::t)))
    | SPow (s1,SFloat (-1.)) -> Format.fprintf f "(1/%a)" (bracket e) s1
    | SPow (s1,s2) -> Format.fprintf f "(%a)^(%a)" (bracket e) s1 (bracket e) s2
    | SMatrix []  -> Format.fprintf f ""
    | SMatrix [h] -> Format.fprintf f "[%a]" print_list h
    | SMatrix (h::t) -> Format.fprintf f "[%a;%a]" print_list h print_list_list t
    | SSin s ->  Format.fprintf f "sin(%a)" (bracket e) s
    | SCos s ->  Format.fprintf f "cos(%a)" (bracket e) s
    | SLog s ->  Format.fprintf f "ln(%a)" (bracket e) s
    | SE -> Format.fprintf f "e"
    | SPI  -> Format.fprintf f "pi"



let rec format_string f e = Format.fprintf f "@{<misc>%s@}" e


let clear_color _ = "\027[38;5;5m\027[0m"

let set_color = function
  | "misc"  -> "\027[38;5;5m\027[0m"  (* blue   *)
  |  "test"  -> "\027[38;5;3m"(* red    *)
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
  Format.printf "   @{<test>%a@}@{<misc>@. @}" formatter e

let print_expr     = make_printer format_expr
let print          = make_printer format_string


let print_intro () = 
 print "\027[38;5;3m
__        __    _  __                       _ 
\\ \\      / /__ | |/ _|_ __ __ _   _ __ ___ | |
 \\ \\ /\\ / / _ \\| | |_| '__/ _` | | '_ ` _ \\| |
  \\ V  V / (_) | |  _| | | (_| |_| | | | | | |
   \\_/\\_/ \\___/|_|_| |_|  \\__,_(_)_| |_| |_|_|";
 print"\n\n\n\027[38;5;1;1mWelcome to Wolfra.ml!\nType help at any time for the help function. Type quit to exit the program.\n\nCopyright (c) 2015 Wolfra.ml Industries\nAll Rights Reserved\n\nThis product is protected by copyright and distributed under\nlicenses restricting copying, distribution and decompilation.\n\n"




let make_string_of f = Format.asprintf "%a" f
let string_of_expr     = make_string_of format_expr
