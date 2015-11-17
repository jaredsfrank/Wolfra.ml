open Ast



module type Monad = sig
 type 'a t
 val bind : 'a t -> ('a -> 'b t) -> 'b t
 val return : 'a -> 'a t
end

module A_Expr : Monad = struct
    type 'a t = 'a * string
    let bind (x,s1) f = 
        let (y,s2) = f x in
        (y,s1^s2)
    let return x = (x, "")
end

let rec subst e = failwith "TODO"

let plus e1 e2 =
    match e1,e2 with
    | Float a, Float b -> Float (a+.b)
    | a, b when a = b  -> BinOp(Times, Float 2., a)
    | BinOp(Times,c1, a), BinOp(Times, c2, b) when a = b -> BinOp(Times, BinOp(Plus, c1, c2),a)
    |  _               -> BinOp (Plus, e1, e2)

let bin_op op e1 e2 =
    match op with
    | Plus   -> plus e1 e2
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
    | Matrix m            -> Matrix m

let rec simplify e = failwith "TODO"
