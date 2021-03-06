open Ast


(*Type of mathematical expressions
 * SFloat 8.5 represents 8.5
 * SVar "x" represents a variable x that is involved in calculus
 * STimes (5., [SVar "x";SVar "y"]) 5*x*y
 * SPlus [SVar "x";SVar "y"] represents x+y
 * SPow (SVar "x"; SVar "y") represents x^y
 * SMatrix [[SVar "a"; SVar "b"];[SVar "c"; SVar "d"]]
        represents a 2x2 matrix with elements a,b,c,d
 * SSin(Var "x") represents sin(x)
 * SCos(Var "x") represents cos(x)
 * SLog(Var "x") represents ln(x)
 * SE represents the number e
 * SPI represents the number pi
*)
type s_expr =
    | SFloat of float
    | SVar of var
    | STimes of float * s_expr list
    | SPlus of s_expr list
    | SPow of s_expr * s_expr
    | SMatrix of s_expr list list
    | SSin of s_expr
    | SCos of s_expr
    | SLog of s_expr
    | SE
    | SPI

(*[plus (s1,s2)] returns s1+s2 in a fully simplified form
 * PRECONDITION: s1 and s2 must be fully simplified
*)
val plus: s_expr * s_expr -> s_expr

(*[times (s1, s2)] returns s1*s2 in a fully simplified form
 * PRECONDITION: s1 and s2 must be fully simplified
*)
val times: s_expr * s_expr -> s_expr

(*[times (s1, s2)] returns s1^s2 in a fully simplified form
 * PRECONDITION: s1 and s2 must be fully simplified
*)
val pow: s_expr * s_expr -> s_expr

(*[times l] returns s1*s2*... for all elements in l
 in a fully simplified form
 * PRECONDITION: All elements of l must be fully simplified
*)

(*[divide (s1, s2)] returns s1/s2 in a fully simplified form
 * PRECONDITION: s1 and s2 must be fully simplified
*)
val divide: s_expr * s_expr -> s_expr

(*[s_times l] returns s1*s2*...sn for all elements in l
 in a fully simplified form
 * PRECONDITION: All elements of l must be fully simplified
*)
val s_times: s_expr list -> s_expr

(*[s_plus l] returns s1+s2+...sn for all elements in l
 in a fully simplified form
 * PRECONDITION: All elements of l must be fully simplified
*)
val s_plus: s_expr list -> s_expr

(*[log_function s] returns log(s) in a fully simplified form
 * PRECONDITION: s must be fully simplified
*)
val log_function: s_expr -> s_expr

(*[sin_function s] returns sin(s) in a fully simplified form
 * PRECONDITION: s must be fully simplified
*)
val sin_function: s_expr -> s_expr

(*[cos_function s] returns cos(s) in a fully simplified form
 * PRECONDITION: s must be fully simplified
*)
val cos_function: s_expr -> s_expr