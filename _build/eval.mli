open Ast


(* [subst ("c", 5) e] substitutes the value 5 for every instance
 *  of the Var or Const c *)
val subst: (var * expr) -> expr -> expr

(** [eval] implements the big-step environment model of evaluation.
 * 
 *  [eval e] returns [v] where
 *  e simplifies to expression v
 *)
val eval: expr -> expr

(* [simplify e] recursively finds sub expressions that within e 
 *  to condense. i.e Plus (Mult(Float 3, Var x), Mult(Float 2, Var x))
 *                  ->    Mult (Float 5, Var x)
 *)
val simplify: expr -> expr
