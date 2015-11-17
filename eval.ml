open Ast


let rec subst = failwith "TODO"

let bin_op op e1 e2 =
    match op with
    | Plus   -> failwith "TODO"
    | Times  -> failwith "TODO"
    | Minus  -> failwith "TODO"
    | Divide -> failwith "TODO"
    | Pow    -> failwith "TODO"
    | Deriv  -> failwith "TODO"
    | Integ  -> failwith "TODO"


let un_op op e =
    match op with
    | Neg       -> failwith "TODO"
    | Sin       -> failwith "TODO"
    | Cos       -> failwith "TODO"
    | Log       -> failwith "TODO"
    | Trans     -> failwith "TODO"
    | Inv       -> failwith "TODO"
    | EigVector -> failwith "TODO"
    | EigValue  -> failwith "TODO"
    | RRef      -> failwith "TODO"


let rec eval = function                   
    | Float  f            -> Float f
    | Var    v            -> Var v
    | Const  c            -> Const c
    | BinOp  (op, e1, e2) -> bin_op op (eval e1) (eval e2)
    | UnOp   (op, e)      -> un_op op (eval e)
    | Matrix m            -> Matrix es

let rec simplify = failwith "TODO"
