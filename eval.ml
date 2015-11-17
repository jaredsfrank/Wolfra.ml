open Ast


let rec subst e = failwith "hey"

let bin_op op e1 e2 =
    match op with
    | Plus   -> failwith "hey"
    | Times  -> failwith "hey"
    | Minus  -> failwith "hey"
    | Divide -> failwith "hey"
    | Pow    -> failwith "hey"
    | Deriv  -> failwith "hey"
    | Integ  -> failwith "hey"


let un_op op e =
    match op with
    | Neg       -> failwith "hey"
    | Sin       -> failwith "hey"
    | Cos       -> failwith "hey"
    | Log       -> failwith "hey"
    | Trans     -> failwith "hey"
    | Inv       -> failwith "hey"
    | EigVector -> failwith "hey"
    | EigValue  -> failwith "hey"
    | RRef      -> failwith "hey"


let rec eval = function                   
    | Float  f            -> Float f
    | Var    v            -> Var v
    | Const  c            -> Const c
    | BinOp  (op, e1, e2) -> bin_op op (eval e1) (eval e2)
    | UnOp   (op, e)      -> un_op op (eval e)
    | Matrix m            -> Matrix es

let rec simplify e = failwith "hey"
