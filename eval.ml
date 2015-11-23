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
    | SMatrix of s_expr list list
    | SSin of s_expr
    | SCos of s_expr
    | SLog of s_expr
    | SE
    | SPI


let rec subst e = failwith "TODO"


(*[is_float s] returns true if s is a float*)
let is_float = function
| SFloat _ -> true
| _ -> false

let not_float x = not (is_float x)


let unbox = function
| SPlus [h] -> h
| STimes [h] -> h
| s -> s


(*Determines if two lists of multiplied terms can be simplified with a new constant coefficient*)
let combinable l1 l2 =
    (List.fold_left (fun accum x -> accum &&(is_float x ||(List.exists (fun a -> a = x) l1))) true l2)
    &&(List.fold_left (fun accum x -> accum &&(is_float x ||(List.exists (fun a -> a = x) l2))) true l1)

(*[combine l1 l2] returns a float with constants from both lists added together*)
let combine (l1: s_expr list) (l2: s_expr list) : s_expr=
    let add_float accum x = (match x with
                           | SFloat f -> accum +. f
                           |  _      -> accum) in

    let accum1 = match List.fold_left add_float 0. l1 with 0. -> 1. | n -> n in
    let accum2 = match List.fold_left add_float 0. l2 with 0. -> 1. | n -> n in
    SFloat (accum1 +. accum2)

(*[compare e1 e2] returns None if e1 and e2 do not combine.
    returns Some e representing the combination of e1 and e2 if they do
*)
let rec compare (e1: s_expr) (e2: s_expr): s_expr option =
    match e1, e2 with
    | a, b when a = b -> Some (times (SFloat 2., e1))
    | SFloat a, SFloat b -> Some (SFloat (a+. b))
    | STimes l1, STimes l2 when combinable l1 l2 ->  Some (times ((combine l1 l2),STimes(List.filter not_float l2)))
    | s, STimes l when combinable [s] l -> Some (times (combine [s] l,STimes (List.filter not_float l) ))
    | STimes l, s when combinable [s] l ->Some(times (combine [s] l,STimes(List.filter not_float l)))
    | _, _ -> None


(*[plus_help l exp] adds exp with l, combining with similar terms if relevant
  INVARIANT: l is fully simplified
*)
and plus_help l exp =
    match l with
    | [] -> [exp]
    | (SPlus l)::t -> Printf.printf "here!!";plus_help(l@t) exp
    | h::t -> (match compare exp h with
                | Some (SFloat 0.) -> t
                | Some e -> e::t
                | None -> h::(plus_help t exp))

(* Returns a fully simplified expression from the added expressions*)
and plus = function
| SFloat a, SFloat b  -> SFloat(a+.b)
| SFloat 0., s        -> s
| s, SFloat 0.        -> s
| SPlus l1, SPlus l2  -> SPlus (List.fold_left plus_help l2 l1)
| SPlus l, s          -> SPlus (plus_help l s)
| s, SPlus l          -> SPlus (plus_help l s)
| s1, s2              -> match compare s1 s2 with Some e -> e | None -> SPlus [s1;s2]


and pow = function
| SFloat 0., SFloat a when a < 0. -> failwith "Division by 0"
| SFloat 0., _                    -> SFloat 0.
| SFloat 1., _                    -> SFloat 1.
| s, SFloat 1.                    -> s
| s, SFloat 0.                    -> SFloat 1.
| SFloat f1, SFloat f2            -> SFloat (f1 ** f2)
| SPlus l, SFloat f when (mod_float f 1. = 0.) && (f >0.) -> (times (SPlus l, pow (SPlus l, SFloat (f-.1.))))
| s1, s2   -> SPow (s1, s2)


and compare_mult (e1: s_expr) (e2: s_expr) : s_expr option =
    let distr s l = List.map (fun x -> times(x,s)) l in
    match e1, e2 with
    | SFloat a, SFloat b ->  Some (SFloat (a *. b))
    | x, SPow(s, y) when x = s    -> Some (pow (s, unbox (plus (y, SFloat 1.))))
    | SPow(s, y), x when x = s    -> Some (pow (s, unbox (plus (y, SFloat 1.))))
    | SPow (s1, x), SPow (s2, y) when s1 = s2 -> Some (pow (s1, unbox (plus (x,y))))
    | SPlus l1, SPlus l2 -> Some (simplify_plus_list (List.fold_left (fun accum x -> accum @ (distr x l2)) [] l1))
    | a, b when a = b -> Some (pow (e1, SFloat 2.))
    | s, SPlus l -> Some( simplify_plus_list (distr s l ))
    | SPlus l, s -> Some( simplify_plus_list (distr s l ))
    | _, _ -> None

and times_help l exp =
    match l, exp with
    | [], _ -> [exp]
    | l1, STimes l2 -> (List.fold_left times_help l1 l2)
    | h::t, _ -> (match compare_mult exp h with
                | Some (SFloat 1.) -> t
                | Some e -> e::t
                | None -> h::(times_help t exp)
                )

and times = function
| SFloat a, SFloat b    -> SFloat(a*.b)
| SFloat 0., _          -> SFloat 0.
| _,  SFloat 0.         -> SFloat 0.
| SFloat 1., s          -> s
| s, SFloat 1.          -> s
| STimes l1, STimes l2  -> STimes (List.fold_left times_help [] (l2@l1))
| STimes l, s           -> STimes (times_help l s)
| s, STimes l           -> STimes (times_help l s)
| s1, s2                -> match compare_mult s1 s2 with Some e -> e | None -> STimes [s1;s2]

and simplify_plus_list l = plus (SPlus l,SPlus [])

and simplify_times_list l = times (STimes l,STimes [])

(*[deriv s1 s2] returns the derivative of s1 with respect to s2*)

(*NOTE: For future, use the functions times, plus, and pow instead of STime, SPlus, and SPow when constructing new
lists of expressions. SPow can literally replace pow. times and plus can only take in two expressions at a time (maybe ill fix this)
So for now, where you would put STimes [e1;e2;e3], put times (e1, times(e2,e3))
*)
let rec deriv s1 s2 =
  match s1, s2 with
  | SFloat x, _ -> SFloat 0.
  | SVar x, SVar x' -> if (x=x') then (SFloat 1.) else SFloat 0.
  | STimes [h], SVar x' -> deriv h s2
  | STimes (h::t), SVar x' -> let l1 = times ((deriv h s2),STimes t) in
                              let l2 = times (h,deriv (STimes t) s2) in
                              plus (l1,l2)
  | SPlus [h], SVar x' -> deriv h s2
  | SPlus (h::t), SVar x'  -> let l1 = deriv h s2 in
                              let l2 = deriv (SPlus t) s2 in
                              plus (l1,l2)
  | SPow (e1, e2), SVar x' -> (match e1 with            (*Note: There are a number of cases I'm not sure about here*)
                              | SE -> times (pow(e1, e2), deriv e2 s2)
                              | SPI -> times (pow (e1, e2), times (SLog e1, deriv e2 s2))
                              | SFloat a -> times ( pow (e1, e2), times( SLog (SFloat a), deriv e2 s2))
                              | SVar x-> if (x = x') then
                                times (e2, times (pow(e1, plus (e2, SFloat (-1.))), deriv e1 s2))
                                else times (pow (e1, e2), times(SLog e1, deriv e2 s2)) 
                              | SPlus x -> failwith "TODO"
                              | STimes x -> failwith "TODO"
                              | _ -> failwith "TODO")
  | SMatrix x, SVar x' -> failwith "TODO"
  | SSin x, SVar x' -> (match x with
              | SFloat v -> SFloat 0.
              | SVar v -> if (v = x') then (SCos x) else (SFloat 0.)
              | _ -> (times ((SCos x),(deriv x s2)) ))
  | SCos x, SVar x' -> (match x with
              | SFloat x -> SFloat 0.
              | SVar v -> if (v = x') then times ((SFloat (-1.)),(SSin x)) else SFloat 0.
              | _ -> times ((SFloat (-1.)), times ((SSin x),(deriv x s2))))
  | SLog x, SVar x' -> (match x with
                   | SFloat v -> SFloat 0.
                   | SVar v -> if (v=x') then (pow (x, SFloat (-1.))) else SFloat 0.
                   | _ -> times ((pow (x, SFloat (-1.))), deriv x s2))

  | SPI, SVar x' -> SFloat 0.
  | SE, SVar x' -> SFloat 0.
  | _, _ -> failwith "This shouldn't happen"



let rec bin_op op s1 s2 =
    match op with
    | Plus   -> plus (s1, s2)
    | Times  -> times (s1, s2)
    | Minus  -> plus (s1, times(s2, SFloat (-1.)))
    | Pow    -> pow (s1, s2)
    | Divide -> times (s1, pow(s2, SFloat (-1.)))
    | Deriv  -> deriv s1 s2

and un_op op s =
    match op with
    | Neg       -> times (SFloat (-1.), s)
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
