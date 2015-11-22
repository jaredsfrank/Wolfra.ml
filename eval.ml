open Ast

module type Monad = sig
 type 'a t
 val bind : 'a t -> ('a -> 'b t) -> 'b t
 val return : 'a -> 'a t
end

module A_Expr = struct
    type 'a t = 'a * string
    let bind (x,s1) f = 
        let (y,s2) = f x in
        (y,s1^s2)
    let return x = (x, "")
end

type s_expr = 
    | SFloat of float
    | SVar of var
    | STimes of s_expr list
    | SPlus of s_expr list
    | SPow of s_expr * s_expr
    | SSin of s_expr
    | SCos of s_expr
    | SLog of s_expr
    | SE
    | SPI


let rec subst e = failwith "TODO"

(*let rec simple_plus = function
| SPlus p -> SPlus (List.filter (fun x -> x <> SFloat 0.) p)
| s       -> s*)

(*[is_float s] returns true if s is a float*)
let is_float = function
| SFloat _ -> true
| _ -> false

let not_float x = not (is_float x)

(*Determines if two lists of multiplied terms can be simplified with a new constant coefficient*)
let combinable l1 l2 = 
    (List.fold_left (fun accum x -> accum &&(is_float x ||(List.exists (fun a -> a = x) l1))) true l2)
    &&(List.fold_left (fun accum x -> accum &&(is_float x ||(List.exists (fun a -> a = x) l2))) true l1)

(*[combine l1 l2] returns a float with constants from both lists added together*)
let combine (l1: s_expr list) (l2: s_expr list) : s_expr= 
    let add_float accum x = (match x with
                           | SFloat f -> accum +. f
                           |  _      -> accum) in
    let accum1 = List.fold_left add_float 0. l1 in
    SFloat (List.fold_left add_float accum1 l2)

(*[compare e1 e2] returns None if e1 and e2 do not combine.
    returns Some e representing the combination of e1 and e2 if they do
*)
let rec compare (e1: s_expr) (e2: s_expr): s_expr option = 
    match e1, e2 with
    | a, b when a = b -> Some (STimes [SFloat 2.; e1])
    | SFloat a, SFloat b -> Some (SFloat (a+. b))
    | STimes l1, STimes l2 when combinable l1 l2 -> Some (STimes ((combine l1 l2):: (List.filter not_float l2)))
    | s, STimes l when combinable [s] l -> Some (STimes ((combine [s] l):: (List.filter not_float l)))
    | STimes l, s when combinable [s] l -> Some (STimes ((combine [s] l):: (List.filter not_float l)))
    | _, _ -> None


(*[add l exp] adds exp with l, combining with similar terms if relevant
  INVARIANT: l is fully simplified
*)
let rec add l exp =
    match l with
    | [] -> [exp]
    | h::t -> (match compare exp h with
                | Some e -> e::t
                | None -> h::(add t exp))

(* Returns a fully simplified expression from the added expressions*)
let plus = function
| SFloat a, SFloat b  -> SFloat(a+.b)
| SPlus l1, SPlus l2  -> SPlus (List.fold_left add l2 l1)
| SPlus l, s          -> SPlus (add l s)
| s, SPlus l          -> SPlus (add l s)
| s1, s2              -> match compare s1 s2 with Some e -> e | None -> SPlus [s1;s2]


let rec simple_mult = function
| STimes p when List.exists (fun x -> x = SFloat 0.) p -> SFloat 0.
| STimes (h::[]) -> h
| STimes p       -> (match (List.filter (fun x -> x <> SFloat 1.) p) with
                    | [] -> SFloat 1.
                    | h::[] -> h
                    | s     -> STimes s)
| s -> s


let times = function
| SFloat a, SFloat b    -> SFloat(a*.b)
| STimes l1, STimes l2  -> STimes (l1 @ l2)
| STimes l, s           -> STimes (s::l)
| s, STimes l           -> STimes (s::l)
| s1, s2                -> STimes [s1;s2]


let multiply (a, b) = simple_mult (times (a,b))

let pow = function
| SFloat 0., SFloat a when a < 0. -> failwith "Division by 0"
| SFloat 0., _                    -> SFloat 0.
| SFloat 1., _                    -> SFloat 1.
| s, SFloat 1.                    -> s
| s, SFloat 0.                    -> SFloat 1.
| s1, s2                          -> SPow (s1, s2)

let rec bin_op op s1 s2 =
    match op with
    | Plus   -> plus (s1, s2)
    | Times  -> multiply (s1, s2)
    | Minus  -> plus (s1, times(s2, SFloat (-1.)))
    | Pow    -> pow (s1, s2)
    | Divide -> multiply (s1, pow(s2, SFloat (-1.)))
    | Deriv  -> failwith "TODO"

and un_op op s =
    match op with
    | Neg       -> multiply (SFloat (-1.), s)
    | Sin       -> SSin s
    | Cos       -> SCos s
    | Log       -> SLog s
    | Trans     -> failwith "TODO"
    | Inv       -> failwith "TODO"
    | EigVector -> failwith "TODO"
    | EigValue  -> failwith "TODO"
    | RRef      -> failwith "TODO"

and eval = function                   
    | Float  f            -> SFloat f
    | Var    v            -> SVar v
    | BinOp  (op, e1, e2) -> bin_op op (eval e1) (eval e2)
    | UnOp   (op, e)      -> un_op op (eval e)
    | Matrix m            -> failwith "TODO"
    | E                   -> SE
    | PI                  -> SPI

let rec simplify e = failwith "TODO"
