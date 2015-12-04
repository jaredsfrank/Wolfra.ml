open Ast
open Simplify
open Matrix
open Derivative
open Integral


let prev = ref (SFloat 42.)
let env = ref ([])


module Logger : (LogMonad with type 'a t = 'a * string list) =
  struct
    type 'a t = 'a * string list
    let bind (x, s1) f = let (y, s2) = f x in (y, s2@s1)
    let (>>=) = bind
    let bind2 (x, s1) (x2, s2) f = let (y, s3) = f x x2 in (y, s3@s2@s1)
    let return x = (x, [])
    let log s = ((), s)
  end

open Logger

let rec subst ((k,v): string * float ) e =
  match e with
  | SFloat f            ->  e
  | SVar x' when x' = k -> SFloat v
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
    | Plus, SMatrix _, _         -> return(matrix_plus (s1, s2))
    | Plus, _, SMatrix _         -> return(matrix_plus (s1, s2))
    | Plus, _, _                 -> return(plus (s1, s2))
    | Times, SMatrix _, _        -> return(matrix_times (s1, s2))
    | Times, _, SMatrix _        -> return(matrix_times (s1, s2))
    | Times, _,_                 -> return(times (s1, s2))
    | Minus, SMatrix _,_         -> return(matrix_plus
                                        (s1, matrix_times(s2, SFloat (-1.))))
    | Minus, _,SMatrix _         -> return(matrix_plus
                                        (s1, matrix_times(s2, SFloat (-1.))))
    | Minus, _,_                 -> return(plus (s1, times(s2, SFloat (-1.))))
    | Pow,_,_                    -> return(pow (s1, s2))
    | Divide,SMatrix _,SMatrix _ -> failwith "Cannot divide matrices"
    | Divide,_,SMatrix _         -> return(matrix_times (s2, pow(s1, SFloat (-1.))))
    | Divide,SMatrix _,_         -> return(matrix_times (s1, pow(s2, SFloat (-1.))))
    | Divide,_,_                 -> return(times (s1, pow(s2, SFloat (-1.))))
    | Deriv,_,_                  -> deriv s1 s2 
    | Integrate,_,_              -> return(integrate s1 s2)
    | Ass, _, _                  -> failwith "Improper Assignment"



let un_op op s =
    match op, s with
    | Neg, _       -> times (SFloat (-1.), s)
    | Sin, _       -> sin_function s
    | Cos, _       -> cos_function s
    | Tan, _       -> times(SSin s, pow(SCos s, SFloat (-1.)))
    | Log, _       -> log_function s
    | Trans, SMatrix m     -> SMatrix(trans_matrix m)
    | Det, SMatrix m when is_square m -> determinant m
    | Det, SMatrix m       -> failwith "Err Square"
    | Inv, SMatrix m when determinant m = SFloat 0. -> failwith "Determinant = 0"
    | Inv, SMatrix m  ->  SMatrix(inv_matrix m)
    | EigVector, SMatrix m -> failwith "TODO"
    | EigValue, SMatrix m  -> SMatrix[[SVar "lambda"];[fst (eigenv m); snd (eigenv m)]]
    | RRef, SMatrix m      -> SMatrix(rref m)
    | _, _      -> failwith "Err Gen"


let rec eval = function
    | Float  f                -> return (SFloat f)
    | Var    v when List.mem_assoc v !env -> return (List.assoc v !env)
    | Var    v                -> return (SVar v)
    | BinOp (Ass, Var x, z)   -> return (env:=(x,fst (eval z))::!env; fst(eval z))
    | BinOp  (op, e1, e2)     ->  bind2 (eval e1) (eval e2) (bin_op op)
    | UnOp   (op, e)          -> return (un_op op (fst (eval e)))
    | Matrix m when is_rect m -> return (
                                SMatrix (List.map (fun l -> List.map (fun x -> fst (eval x)) l) m))
    | Matrix m                -> failwith "Improper matrix"
    | Subst  (Float f,Var v,e)      -> return (subst (v,f) (fst (eval e)))
    | Subst (Var v, Float f, e)     -> return (subst (v,f) (fst (eval e)))
    | Subst (_)               -> failwith "Cannot substitute that"
    | Taylor (f, e1, e2)      -> return (taylor f (fst (eval e2)) (fst(eval e1)))
    | E                       -> return (SE)
    | PI                      -> return (SPI)
    | Ans                     -> return (!prev)
