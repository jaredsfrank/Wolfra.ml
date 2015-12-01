open Ast

type s_expr =
    | SFloat of float
    | SVar of var
    | STimes of float * s_expr list
    | SPlus of s_expr list
    | SPow of s_expr * s_expr
    | SMatrix of s_expr list list
    | SSin of s_expr
    | SCos of s_expr
    | STan of s_expr 
    | SLog of s_expr
    | SE
    | SPI

val plus: s_expr * s_expr -> s_expr
val times: s_expr * s_expr -> s_expr
val pow: s_expr * s_expr -> s_expr
val s_times: s_expr list -> s_expr
val s_plus: s_expr list -> s_expr