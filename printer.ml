open Simplify
open Fractions

let string_of_floats f =
    let s = string_of_float f in
    if String.get s ((String.length s)-1) = '.' then String.sub s 0 ((String.length s)-1)
    else s


let rec format_expr f e = 
    let rec print_list f = function
        | [] -> Format.fprintf f ""
        | [h] -> Format.fprintf f "%a" format_expr h
        | h::t -> Format.fprintf f "%a,%a" format_expr h (print_list) t
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
    | STimes (1.,[h]) -> Format.fprintf f "%a" format_expr h
    | STimes (-1.,[h]) -> Format.fprintf f "-%a" format_expr h
    | STimes (c,[SPow(x, SFloat (-1.))]) -> Format.fprintf f "%a/%a" (print_frac) c format_expr x
    | STimes (c,[h]) -> Format.fprintf f "%a%a" (print_frac) c format_expr h
    | STimes (-1., h::(SPow (e, SFloat (-1.)))::t) -> Format.fprintf f "-%a/(%a)" format_expr h format_expr (STimes (1.,e::t))
    | STimes (1., h::(SPow (e, SFloat (-1.)))::t) -> Format.fprintf f "%a/(%a)" format_expr h format_expr (STimes (1.,e::t))
    | STimes (c, h::(SPow (e, SFloat (-1.)))::t) -> Format.fprintf f "%a%a/(%a)" (print_frac) c format_expr h format_expr (STimes (1.,e::t))
    | STimes (-1., h::t) -> Format.fprintf f "-%a*%a" format_expr h format_expr (STimes (1.,t))
    | STimes (1., h::t) -> Format.fprintf f "%a*%a" format_expr h format_expr (STimes (1.,t))
    | STimes (c, h::t) -> Format.fprintf f "%a%a*%a" (print_frac) c format_expr h format_expr (STimes (1.,t))
    | SPlus [] -> Format.fprintf f ""
    | SPlus [h] -> Format.fprintf f "%a" format_expr h
    | SPlus (h1::h2::t) -> (match h2 with 
                              | SFloat x when x<0. -> Format.fprintf f "%a-%a" format_expr h1 format_expr (SPlus ((SFloat (-1.*.x))::t))
                              | STimes (c,x) when c<0. -> Format.fprintf f "%a-%a" format_expr h1 format_expr (SPlus (STimes (-1.*.c,x)::t))
                              | _ -> Format.fprintf f "%a+%a" format_expr h1 format_expr (SPlus( h2::t)))
    | SPow (s1,SFloat (-1.)) -> Format.fprintf f "(1/%a)" format_expr s1
    | SPow (s1,s2) -> Format.fprintf f "(%a)^(%a)" format_expr s1 format_expr s2
    | SMatrix []  -> Format.fprintf f ""
    | SMatrix [h] -> Format.fprintf f "[%a]" print_list h
    | SMatrix (h::t) -> Format.fprintf f "[%a;%a]" print_list h print_list_list t
    | SSin s ->  Format.fprintf f "sin(%a)" format_expr s
    | SCos s ->  Format.fprintf f "cos(%a)" format_expr s
    | SLog s ->  Format.fprintf f "ln(%a)" format_expr s
    | SE -> Format.fprintf f "e"
    | SPI  -> Format.fprintf f "pi"



let rec format_string f e = Format.fprintf f "@{<misc>%s@}" e


let clear_color _ = "\027[38;5;5m\027[0m"

let set_color = function
  | "misc"  -> "\027[38;5;5m\027[0m"  (* white   *)
  |  "test"  -> "\027[38;5;3m"(* red    *)
  | "yellow" -> "\027[38;5;3m"
  |   _     -> "\027[38;5;5m\027[0m"

(*The following two functions are mostly taken directly from A4*)

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
 print"\n\n\n\027[38;5;1;1m Welcome to Wolfra.ml!
 Type help at any time for the help function. Type quit to exit the program.

 Copyright (c) 2015 Wolfra.ml Industries
 All Rights Reserved

 This product is protected by copyright and distributed under
 licenses restricting copying, distribution and decompilation.\n\n"


let print_help () =
  print
  "\027[38;5;3m_   _      _         __  __                  
  | | | | ___| |_ __   |  \\/  | ___ _ __  _   _ 
  | |_| |/ _ \\ | '_ \\  | |\\/| |/ _ \\ '_ \\| | | |
  |  _  |  __/ | |_) | | |  | |  __/ | | | |_| |
  |_| |_|\\___|_| .__/  |_|  |_|\\___|_| |_|\\__,_|
               |_|                              
   "

let print_main_help () =
   print "\027[38;5;1;1mThis is a Symbolic Computation System inspired by WolframAlpha

   The program has 3 main features: 
     *   Derivatives, Integration, Matrices

     Type one of the following to learn more:
      *  Basic Operation
      *  Derivatives
      *  Integrals
      *  Matrices

     OR press ENTER to return

  "

  let print_basic_help () =
    print "\027[38;5;3m BASIC OPERATION
    \027[38;5;1;1m
    Variables: Any combination of adjacent letters
    Reserved Variables: pi, e, c, [any word that is used as a command]
    Assignment: Typing [var] = [expr] assigns that expression to the variable
                Typing 'clear' removes all previously made assignments.
                EX: f = x^3 [ENTER] f + 2x => x^3 + 2x

    Example Entries and Results (Signified by =>)
    * Multiplication:3*5 => 15,  x*y => x*y, 3x => 3x
    * Addition:      3+5 => 8,   x+y => x+y, 2*x - x => x
    * Subtraction:   3-5 => -2,  2-x => 2-x
    * Division:      3/5 => 3/5, x/y => x/y, x/x => 1
    * Exponents:     3^2 => 9,   x^y = (x)^(y)
    * Trig:          cos(pi) => -1, sin(x) => sin(x), tan(x) => sin(x)/cos(x)
    * Natural Log:   log(e) => 1, log(x) => log(x)
    * Distribution:  (x+y)^2 => 2y*x+(y)^(2)+(x)^(2)
    * Substitution:  sub 3 for x in y*x^2 => 9y
    * Previous Ans:  5x [ENTER] Ans + x => 6x
   
    To return to the main help menu, type MAIN
    OR press ENTER to return"

  let print_deriv_help () = 
    print "\027[38;5;3m DERIVATIVES
    \027[38;5;1;1m
    Derivatives for all types of expressions are fully functional

    There are 3 notations for denoting a derivative:
    (1) derive [expr] wrt [var]
        EX: derive x^3 wrt x => 3x^2
    (2) d([expr])/d[var]
        EX: d(sin(x*y))/dx => cos(x*y)*y
    (3) [var] = [expr] [ENTER] [var][' or '' or '''][var2]
        f = x^5
        f'(x) = 5x^4
        f''(x) = 20x^3
        f'''(x) = 60x^2

    More Complex Example
      derive x^y*cos(x) wrt x => y*(x)^(y-1)*cos(x)-sin(x)*(x)^(y)



    To return to the main help menu, type MAIN
    OR press ENTER to return"

  let print_integ_help () =
    print "\027[38;5;3m DERIVATIVES
    \027[38;5;1;1m
    Integrals are currently in beta...not fully functional.
    Fully supported integrals:
      (where [c] represents any non-variable expr
       and [x] represents the integrating variable
      [i] = [c] | [x] | [c]/[x] | e^([c]*x) | [x]^[c] | 1/([c][x]) | sin([c][x])
            | cos([c][x]) | log([c][x]) | [i]+[i]
    Not supported integrals:
      Any integral requiring u substitution that is not explicitly supported
      or that may lead to inverse trig.
    Semi Supported:
      Some By-Parts integration: Highlights: x*sin(2x), (e^x)*x, cos(x)*x^2

    Notation:
      integrate [expr] wrt [var] EX: integrate x^3 wrt x => (1/3)x^3
      Multivariable integration can be accomplished as follows:
        integrate integrate x*y wrt x wrt y
      Definite integrals are not explicitly supported but are easily solved for:
        f = integrate x^3 wrt x [ENTER] (sub 5 for x in f) - (sub 3 for x in f)

    To return to the main help menu, type MAIN
    OR press ENTER to return"

  let print_matr_help () =
    print "\027[38;5;3m MATRICES
    \027[38;5;1;1m
    Matrices are denoted in the following way:
      Columns are separated by commaas. Rows are separated by semicolons
      EX: To express [1 3], write [1,3;5,6] 
                     [5 6]

    Vectors are denoted by a list of expressions separated by semicolons
      EX: 3i + 5j + 6k is written as [3;5;6]

    Multiplication: [1,2;3,4] * [a,b;c,d] => [a+2c,b+2d;3a+4c,3b+4d]
    Addition:       [1,2;3,4] + [a,b;c,d] => [1+a,2+b;3+c,4+d]
    Transpose:      [a,b;c,d] => [a,c;b,d]
    Inverse:        inverse [1,2,3;0,1,4;5,6,0] => [-24,18,5;20,-15,-4;-5,4,1]
    Reduce Row Ech: RREF [1,2,3;4,5,6] => [1,0,-1;-0,1,2]
    
    EigenValues: (Only 2x2) are supported
      EX: eigenvalue [2,7;-1,6] => [lambda;1,-5]
        This result represnts the two eigenvalues: 1, and -5


    To return to the main help menu, type MAIN
    OR press ENTER to return"



let make_string_of f = Format.asprintf "%a" f
let string_of_expr     = make_string_of format_expr
