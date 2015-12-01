open Simplify

(*[is_squre m] returns true iff every row of m has the same length as the the length of m*)
val is_square: s_expr list list -> bool

(*[is_rect m] returns true iff every row is of equal length*)
val is_rect: 'a list list -> bool

(*[matrix_plus (s1,s2)] returns a matrix representing s1+s2 
    PRECONDITION: s1 and s2 are of identical dimensions
*)
val matrix_plus: s_expr * s_expr -> s_expr

(*[matrix_times (s1, s2) returns a a matrix representing s1*s2 
    PRECONDITION: if dimensions of s1 are m*n, s2 must be n*l
*)
val matrix_times: s_expr * s_expr -> s_expr

(*[rref m] returns the reduced row echelon form of m*)
val rref: s_expr list list -> s_expr list list

(*[rref m] returns the inverse of m*)
val inv_matrix: s_expr list list -> s_expr list list

(*[rref m] returns the transpose of m*)
val trans_matrix: s_expr list list -> s_expr list list

(*[rref m] returns the determinant of m*)
val determinant: s_expr list list -> s_expr
