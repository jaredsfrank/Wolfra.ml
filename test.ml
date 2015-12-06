open Parse
open Simplify
open Eval
open Printer
open Assertions

let lists_equal l1 l2 =
    let b1 = List.fold_left (fun accum x -> accum &&  List.exists (fun y -> y = x) l2) true l1 in
    let b2 = List.fold_left (fun accum x -> accum && List.exists (fun y -> y = x) l1) true l2 in
    b1 && b2 && (List.length l1 = List.length l2)

let evaluate s = string_of_expr(eval(parse_expr s))
let eval_ast s = eval(parse_expr s)


(*Times tests*)
TEST "times Const" = evaluate "5x" = "5x"
TEST "Basic times" = evaluate "x*y" = "x*y"

(*NOTE:THIS IS ACTUALLY A GREAT WAY TO TEST.
TESTING PROPERTIES JUST REQUIRES THAT THE TWO EQUIVALENT EXPRESSIONS EVLAUATE TO THE SAME VALUE*)
TEST "Add Zero" = evaluate "x+0" = evaluate "x"
TEST "Mult Zero" = evaluate "x*0" = evaluate "0"
TEST "Mult One" = evaluate "x*1" = evaluate "x"
TEST "Add two floats" = evaluate "1 + 2.5" = evaluate "3.5"
TEST "Zero to power" = evaluate "0^x" = evaluate "0"
TEST "One to power" = evaluate "1^x" = evaluate "1"
TEST "Power of one" = evaluate "x^1" = evaluate "x"
TEST "Power of zero" = evaluate "x^0" = evaluate "1"
TEST "Power of floats" = evaluate "3^4" = evaluate "81"
TEST "power of summed terms" = evaluate "(x+y)^3"
                    = evaluate "3*(y^2)*x+(y^3)+3y*(x^2)+(x^3)"
TEST "power of mulitplied terms" = evaluate "(x*y)^3" = evaluate "(x^3)*(y^3)"
TEST "power of time list" = evaluate "(x*y*2)^2" = evaluate "4*(y^2)*(x^2)"
TEST "divide" = evaluate "x/y" = evaluate "x*(y^-1)"
TEST "subtration of same expressions" = evaluate"x*y - y*x" = evaluate "0"
TEST "Times pows" = evaluate "x^(-5)*x^(2)" = evaluate "x^(-3)"
TEST "Adding combinable expressions" = evaluate "2*x+x*3" = evaluate "5*x"
TEST "FOLDING" =  evaluate "(z+w)*(x+y)" = evaluate "x*w+ y*w + x*z + y*z"
TEST "Distributive" = evaluate "2*(x+y)" = evaluate "2*x+2*y"
TEST "Log of E" = evaluate "log(e^x)" = evaluate "x"
TEST "Log of power" = evaluate "log(x^y)" = evaluate "y*log(x)"
TEST "E pow of Log" = evaluate "e^log(x)" = evaluate "x"
TEST "E pow of Log2" = evaluate "e^(2*log(x))" = evaluate "x^2"
TEST "Sin of PI" = evaluate "sin(pi)" = evaluate "0"
TEST "Cos of PI" = evaluate "cos(pi)" = evaluate "-1"
TEST "Cos of zero" = evaluate "cos(0)" = evaluate "1"
TEST "Cos of half PI" = evaluate "cos(0.5*pi)" = evaluate "0"
TEST "Cos of an even factor of PI" = evaluate "cos(4*pi)" = evaluate "1"
TEST "Cos of an odd factor of PI" = evaluate "cos(3*pi)" = evaluate "-1"
TEST "Sin of half PI1" = evaluate "sin(0.5*pi)" = evaluate "1"
TEST "Sin of half PI2" = evaluate "sin(1.5*pi)" = evaluate "-1"
TEST "Sin of multiple pi" = evaluate "sin(7*pi)" = evaluate "0"



TEST "Distributive" =
    match eval_ast "(x+1)^2" with
    | SPlus l when lists_equal l [STimes(2.,[SVar "x"]);SFloat 1.; SPow(SVar "x",SFloat 2.)] -> true
    | _ -> false

TEST "Distributive 2" =
    evaluate "(x+y)*(x+y)*(x+y)*(x+y)" = evaluate "(x+y)^4"


TEST "Derivative Simple" = evaluate "derive x^2 wrt x" = "2x"
TEST "Derivative Exp" = evaluate "derive x^y wrt x" = "y*(x)^(y-1)"
TEST "Derivative Times" = evaluate "derive x*sin(x) wrt x" = "sin(x)+x*cos(x)"

TEST "Integration Simple" = evaluate "integrate 2*x^4 wrt x" = "(2/5)(x)^(5)+C"
