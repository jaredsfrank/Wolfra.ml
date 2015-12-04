open Ast
open Simplify

(* The previous value computed *)
val prev: s_expr ref

val env: (string * s_expr) list ref

(* [subst ("c", 5) e] substitutes the value 5 for every instance
 *  of the Var or Const c *)
val subst: (string * float) -> s_expr -> s_expr

(** [eval] implements the big-step environment model of evaluation.
 * 
 *  [eval e] returns [v] where
 *  e simplifies to expression v
 *)
val eval: expr -> s_expr
