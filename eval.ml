open Ast
open Simplify
open Matrix
open Derivative
open Integral

let prev = ref (SFloat 42.)
let env = ref ([])

let rec subst ((k,v): string * s_expr ) e =
  match e with
  | SFloat f            ->  e
  | SVar x' when x' = k -> v
  | SVar x              -> e
  | STimes (c, [])      -> SFloat c
  | STimes (c, h::t)    -> times(subst (k,v) h, subst (k,v) (STimes(c,t)))
  | SPlus []            -> SFloat 0.
  | SPlus (h::t)        -> plus(subst (k,v) h, subst (k,v) (SPlus t))
  | SPow (e1,e2)        -> pow (subst (k,v) e1, subst  (k,v) e2)
  | SMatrix m           -> SMatrix (List.map
                           (fun l -> List.map (subst (k,v)) l) m)
  | SSin x              -> sin_function (subst (k,v) x)
  | SCos x              -> cos_function (subst (k,v) x)
  | SLog x              -> log_function (subst (k,v) x)
  | SE                  -> SE
  | SPI                 -> SPI

let rec bin_op op s1 s2 =
    match op, s1, s2 with
    | Plus, SMatrix _, _         -> matrix_plus (s1, s2)
    | Plus, _, SMatrix _         -> matrix_plus (s1, s2)
    | Plus, _, _                 -> plus (s1, s2)
    | Times, SMatrix _, _        -> matrix_times (s1, s2)
    | Times, _, SMatrix _        -> matrix_times (s1, s2)
    | Times, _,_                 -> times (s1, s2)
    | Minus, SMatrix _,_         -> matrix_plus
                                    (s1, matrix_times(s2, SFloat (-1.)))
    | Minus, _,SMatrix _         -> matrix_plus
                                    (s1, matrix_times(s2, SFloat (-1.)))
    | Minus, _,_                 -> plus (s1, times(s2, SFloat (-1.)))
    | Pow,_,_                    -> pow (s1, s2)
    | Divide,SMatrix _,SMatrix _ -> failwith ("Error: "^"
                                    You cannot divide two matrices")
    | Divide,_,SMatrix _         -> matrix_times (s2, pow(s1, SFloat (-1.)))
    | Divide,SMatrix _,_         -> matrix_times (s1, pow(s2, SFloat (-1.)))
    | Divide,_,_                 -> times (s1, pow(s2, SFloat (-1.)))
    | Deriv,_,_                  -> deriv s1 s2
    | Integrate,_,_              -> plus(integrate s1 s2, SVar "C")
    | Ass, _, _                  -> failwith ("Error: This is an improper"^
                                    " assignment")



let un_op op s =
    match op, s with
    | Neg, _       -> times (SFloat (-1.), s)
    | Sin, _       -> sin_function s
    | Cos, _       -> cos_function s
    | Tan, _       -> divide(sin_function s, cos_function s)
    | Log, _       -> log_function s
    | Trans, SMatrix m     -> SMatrix(trans_matrix m)
    | Det, SMatrix m when is_square m -> determinant m
    | Det, SMatrix m       -> failwith "Error: The matrix must be square"
    | Inv, SMatrix m when determinant m = SFloat 0. ->
                              failwith "Error: The determinant is 0"
    | Inv, SMatrix m  ->  SMatrix(inv_matrix m)
    | EigValue, SMatrix m  -> SMatrix[[SVar "lambda"];
                              [fst (eigenv m); snd (eigenv m)]]
    | RRef, SMatrix m      -> SMatrix(rref m)
    | _, _      -> failwith ("Error: This operation may only be performed "^
                    "on matrices")

let rec eval = function
    | Float  f                -> SFloat f
    | Var    v when List.mem_assoc v !env -> List.assoc v !env
    | Var    v                -> SVar v
    | BinOp (Ass, Var x, z)   -> env:=(x,(eval z))::!env; (eval z)
    | BinOp  (op, e1, e2)     -> bin_op op (eval e1) (eval e2)
    | UnOp   (op, e)          -> un_op op (eval e)
    | Matrix m when is_rect m -> SMatrix (List.map (fun l -> List.map eval l) m)
    | Matrix m                -> failwith "Error: Matrix must be rectangular"
    | Subst (x, Var v, e)     -> subst (v,eval x) (eval e)
    | Subst (_)               -> failwith "Error: Improper substitution"
    | E                       -> SE
    | PI                      -> SPI
    | Ans                     -> !prev
