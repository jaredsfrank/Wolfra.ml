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



TEST "Distributive" = 
    match eval_ast "(x+1)^2" with 
    | SPlus l when lists_equal l [STimes(2.,[SVar "x"]);SFloat 1.; SPow(SVar "x",SFloat 2.)] -> true
    | _ -> false


TEST "Derivative Simple" = evaluate "derive x^2 wrt x" = "2x"
TEST "Derivative Exp" = evaluate "derive x^y wrt x" = "y*(x)^(y-1)"
TEST "Derivative Times" = evaluate "derive x*sin(x) wrt x" = "sin(x)+x*cos(x)"
