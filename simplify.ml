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
    | SLog of s_expr
    | SE
    | SPI

let unbox = function
    | SPlus [h] -> h
    | STimes (1.,[h]) -> h
    | STimes (0.,x) -> SFloat 0.
    | STimes (x,[]) -> SFloat x
    | s -> s


(*Determines if two lists of multiplied terms can be simplified with a new constant coefficient*)
let combinable l1 l2 =
  let same l1' l2' = List.fold_left (fun accum x -> accum &&(List.exists (fun a -> a = x) l1')) true l2' in
  same l1 l2 && same l2 l1


(*[compare e1 e2] returns None if e1 and e2 do not combine.
    returns Some e representing the combination of e1 and e2 if they do
*)
let rec compare (e1: s_expr) (e2: s_expr): s_expr option =
    match e1, e2 with
    | a, b when a = b -> Some (times (SFloat 2., e1))
    | SFloat a, SFloat b -> Some (SFloat (a+. b))
    | STimes (c1,l1), STimes (c2,l2) when combinable l1 l2 ->  Some (STimes (c1+.c2,l1))
    | s, STimes (c,l) when combinable [s] l -> Some (unbox (STimes (1.+.c,l)))
    | STimes (c,l), s when combinable [s] l -> Some (unbox (STimes (1.+.c,l)))
    | _, _ -> None


(*[plus_help l s] adds exp with s, combining with similar terms if relevant
  INVARIANT: l is fully simplified
*)
and plus_help l s =
    match l with
    | [] -> [s]
    | (SPlus l)::t -> plus_help(l@t) s
    | h::t -> (match compare s h with
                | Some (SFloat 0.) -> t
                | Some e -> e::t
                | None -> h::(plus_help t s))
                

(* Returns a fully simplified expression from the added expressions*)
and plus = function
  | SFloat a, SFloat b  -> SFloat(a+.b)| SFloat 0., s        -> s
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
  | STimes (c,[h]), x -> times(pow(SFloat c,x),pow(h,x))
  | STimes (c,h::t), x -> times(pow(h,x),pow(STimes (c,t),x))
  | s1, s2   -> SPow (s1, s2)



and compare_mult (e1: s_expr) (e2: s_expr) : s_expr option =
    let distr s l = List.fold_left (fun accum x -> plus(times(x,s), accum)) (SPlus []) l in
    match e1, e2 with
    | x, SPow(s, y) when x = s    -> Some (pow (s, unbox (plus (y, SFloat 1.))))
    | SPow(s, y), x when x = s    -> Some (pow (s, unbox (plus (y, SFloat 1.))))
    | SPow (s1, x), SPow (s2, y) when s1 = s2 -> Some (pow (s1, unbox (plus (x,y))))
    | SPlus l1, SPlus l2 -> Some ( (List.fold_left (fun accum x -> plus(accum, (distr x l2))) (SPlus []) l1))
    | a, b when a = b -> Some (pow (e1, SFloat 2.))
    | s, SPlus l -> Some(distr s l )
    | SPlus l, s -> Some(distr s l )
    | _, _ -> None

  and times_help l exp =
    match l, exp with
    | [], _ -> [exp]  
    | h::t, _ -> (match compare_mult exp h with
                | Some (SFloat 1.) -> t
                | Some e -> e::t
                | None -> h::(times_help t exp)
                )

and times (e1,e2) =
  match e1,e2 with
  | SFloat a, STimes(c,l) -> unbox(STimes (c*.a,l))| STimes(c,l), SFloat a -> unbox(STimes (c*.a,l))
  | SFloat a, SFloat b    -> SFloat(a*.b)
  | SFloat a, e           -> unbox(match compare_mult e1 e2 with Some e -> e | None -> STimes (a,[e]))
  | e, SFloat a           -> unbox(match compare_mult e1 e2 with Some e -> e | None -> STimes (a,[e]))
  | STimes (c1,l1), STimes (c2,l2)  -> unbox(STimes (c1*.c2,List.fold_left times_help l1 l2))
  | STimes (c,l), _         -> unbox(STimes (c,times_help l e2))
  | _, STimes (c,l)         -> unbox(STimes (c,times_help l e1))
  | s1, s2                -> unbox(match compare_mult s1 s2 with Some e -> e | None -> STimes (1.,[s1;s2]))

and s_times l = unbox(List.fold_left (fun a b -> times (a,b)) (STimes (1.,[])) l)
and s_plus l = unbox(List.fold_left (fun a b -> plus (a,b)) (SPlus []) l)


