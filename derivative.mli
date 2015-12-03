open Simplify

(*[deriv s1 s2] returns the derivative of s1 with respect to s2*)
val deriv: s_expr -> s_expr -> s_expr

(*[multi_deriv n s1 s2] performs n derivatives of s1 with respct to s2 *)
val multi_deriv: float -> s_expr -> s_expr -> s_expr

val taylor: float -> s_expr -> s_expr -> s_expr