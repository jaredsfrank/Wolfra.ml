open Ast


type s_expr = 
    | SFloat of float
    | SVar of var
    | STimes of float* s_expr list
    | SPlus of s_expr list
    | SPow of s_expr * s_expr
    | SMatrix of s_expr list list
    | SSin of s_expr
    | SCos of s_expr
    | SLog of s_expr
    | SE
    | SPI

(* [subst ("c", 5) e] substitutes the value 5 for every instance
 *  of the Var or Const c *)
val subst: (string * float) -> s_expr -> s_expr

(** [eval] implements the big-step environment model of evaluation.
 * 
 *  [eval e] returns [v] where
 *  e simplifies to expression v
 *)
val eval: expr -> s_expr
