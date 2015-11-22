open Ast


type s_expr = 
    | SFloat of float
    | SVar of var
    | STimes of s_expr list
    | SPlus of s_expr list
    | SPow of s_expr * s_expr
    | SMatrix of s_expr list
    | SSin of s_expr
    | SCos of s_expr
    | SLog of s_expr
    | SE
    | SPI

(* [subst ("c", 5) e] substitutes the value 5 for every instance
 *  of the Var or Const c *)
val subst: (var * expr) -> expr -> expr

(** [eval] implements the big-step environment model of evaluation.
 * 
 *  [eval e] returns [v] where
 *  e simplifies to expression v
 *)
val eval: expr -> s_expr

(* [simplify e] recursively finds sub expressions that within e 
 *  to condense. i.e Plus (Mult(Float 3, Var x), Mult(Float 2, Var x))
 *                  ->    Mult (Float 5, Var x)
 *)
val simplify: expr -> expr
