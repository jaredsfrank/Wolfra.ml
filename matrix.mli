open Simplify

val is_square: s_expr list list -> bool
val is_rect: 'a list list -> bool
val matrix_plus: s_expr * s_expr -> s_expr
val matrix_times: s_expr * s_expr -> s_expr
val rref: s_expr list list -> s_expr list list
val inv_matrix: s_expr list list -> s_expr list list
val trans_matrix: s_expr list list -> s_expr list list
val determinant: s_expr list list -> s_expr
