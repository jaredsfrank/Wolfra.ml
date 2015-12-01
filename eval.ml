open Ast
open Simplify
open Matrix
open Derivative
open Integral


let rec subst ((k,v): string * float ) e =
  match e with
  | SFloat f ->  e
  | SVar x' when x' = k -> SFloat v
  | SVar x -> e
  | STimes (c, []) -> SFloat c
  | STimes (c, h::t) -> times(subst (k,v) h, subst (k,v) (STimes(c,t)))
  | SPlus [] -> SFloat 0.
  | SPlus (h::t) -> plus(subst (k,v) h, subst (k,v) (SPlus t))
  | SPow (e1,e2) -> pow (subst (k,v) e1, subst  (k,v) e2)
  | SMatrix m -> SMatrix (List.map (fun l -> List.map (subst (k,v)) l) m)
  | SSin x -> SSin (subst (k,v) x)
  | SCos x -> SCos (subst (k,v) x)
  | STan x -> STan (subst (k,v) x)
  | SLog x -> SLog (subst (k,v) x)
  | SE -> SE
  | SPI -> SPI

let rec bin_op op s1 s2 =
    match op, s1, s2 with
    | Plus, SMatrix _, _        -> matrix_plus (s1, s2)
    | Plus, _, SMatrix _        -> matrix_plus (s1, s2)
    | Plus, _, _                -> plus (s1, s2)
    | Times, SMatrix _, _        -> matrix_times (s1, s2)
    | Times, _, SMatrix _        -> matrix_times (s1, s2)
    | Times, _,_                 -> times (s1, s2)
    | Minus, SMatrix _,_         -> matrix_plus (s1, matrix_times(s2, SFloat (-1.)))
    | Minus, _,SMatrix _         -> matrix_plus (s1, matrix_times(s2, SFloat (-1.)))
    | Minus, _,_                 -> plus (s1, times(s2, SFloat (-1.)))
    | Pow,_,_                    -> pow (s1, s2)
    | Divide,SMatrix _,SMatrix _ -> failwith "Cannot divide matrices"
    | Divide,_,SMatrix _         -> matrix_times (s2, pow(s1, SFloat (-1.)))
    | Divide,SMatrix _,_         -> matrix_times (s1, pow(s2, SFloat (-1.)))
    | Divide,_,_                 -> times (s1, pow(s2, SFloat (-1.)))
    | Deriv,_,_                  -> deriv s1 s2
    | Integrate,_,_              -> integrate s1 s2



let un_op op s =
    match op, s with
    | Neg, _       -> times (SFloat (-1.), s)
    | Sin, _       -> SSin s
    | Cos, _       -> SCos s
    | Tan, _       -> STan s
    | Log, _       -> SLog s
    | Trans, SMatrix m     -> SMatrix(trans_matrix m)
    | Det, SMatrix m when is_square m -> determinant m 
    | Det, SMatrix m       -> failwith "Err Square"
    | Inv, SMatrix m when determinant m = SFloat 0. -> failwith "Determinant = 0"
    | Inv, SMatrix m  ->  SMatrix(inv_matrix m)
    | EigVector, SMatrix m -> failwith "TODO"
    | EigValue, SMatrix m  -> failwith "TODO"
    | RRef, SMatrix m      -> SMatrix(rref m)
    | _, _      -> failwith "Err Gen"


let rec eval = function
    | Float  f                -> SFloat f
    | Var    v                -> SVar v
    | BinOp  (op, e1, e2)     -> bin_op op (eval e1) (eval e2)
    | UnOp   (op, e)          -> un_op op (eval e)
    | Matrix m when is_rect m -> SMatrix (List.map (fun l -> List.map eval l) m)
    | Matrix m                -> failwith "Improper matrix"
    | Subst  (Float f,Var v,e)      -> subst (v,f) (eval e)
    | Subst (Var v, Float f, e)     -> subst (v,f) (eval e)
    | Subst (_)               -> failwith "Cannot substitute that"
    | E                       -> SE
    | PI                      -> SPI

