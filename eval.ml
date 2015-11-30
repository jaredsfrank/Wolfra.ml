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
    | STimes of float * s_expr list
    | SPlus of s_expr list
    | SPow of s_expr * s_expr
    | SMatrix of s_expr list list
    | SSin of s_expr
    | SCos of s_expr
    | SLog of s_expr
    | SE
    | SPI





(*[is_float s] returns true if s is a float*)
let is_float = function
| SFloat _ -> true
| _ -> false

let not_float x = not (is_float x)


let unbox = function
    | SPlus [h] -> h
    | STimes (1.,[h]) -> h
    | STimes (0.,x) -> SFloat 0.
    | STimes (x,[]) -> SFloat x
    | s -> s


(* Checks a matrix's lists to make sure each row has the same number of columns*)
let rec check_dim m f =
    match m with
    | [] -> true
    | h::t -> if List.length h = f then check_dim t f
              else false

(* Checks two matrices' lists to make sure their dimensions are the same*)
let rec check_dimension m n =
    match m, n with
    | [], [] -> true
    | h1::t1, h2::t2 -> if List.length h1 = List.length h2 then check_dimension t1 t2
                        else false
    | [], _ -> false
    | _, [] -> false

(*Creates the s_expr list list of dimensions row col with expression f in every position*)
let create_matrix row col f =
    let rec make_rows c acc = if c = 0 then acc else make_rows (c-1) (f::acc) in
    let rows = make_rows col [] in
    let rec combine_rows r acc = if r = 0 then acc else combine_rows (r-1) (rows::acc) in
    combine_rows row []

(* Returns the transpose of a matrix*)
let rec trans_matrix = function
  | [] -> []
  | []::t -> trans_matrix t
  | (h::t1)::t2 -> (h::(List.map List.hd t2))::trans_matrix (t1::(List.map List.tl t2))
  


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
                

(*Adds one matrices lists to the other, takes in the list list instead of the SMatrix type*)
(*NOTE: the 'and's mean tht the functions are dependently recursive so you can't just stick
other functions between them. That is why it didn't compile
This function does belong here becuase it relies on plus and plus
relies on it. However, I have stuck the other functions higher up.*)
and scalar_matrices f m n =
    match m, n with
    | [], [] -> []
    | h1::t1, h2::t2 -> let rec helper l1 l2 =
                        (match l1, l2 with
                        | [], [] -> []
                        | s1::d1, s2::d2 -> (f (s1,s2))::(helper d1 d2)
                        | _, _ -> failwith "Not correct # of columns")
                        in (helper h1 h2)::(scalar_matrices f t1 t2)
    | _, _ -> failwith "Not correct # of rows"

(* Returns a fully simplified expression from the added expressions*)
and plus = function
| SFloat a, SFloat b  -> SFloat(a+.b)
| SMatrix m, SFloat f -> if (check_dim m (List.length (List.hd m))) then
                           (let fmatrix = create_matrix (List.length m)
                           (List.length (List.hd m)) (SFloat f) in
                             SMatrix(scalar_matrices plus m fmatrix))
                         else failwith "Not correct dimensions"
| SFloat f, SMatrix m -> plus (SMatrix m, SFloat f)
| SMatrix m, SMatrix n ->
                  if (check_dimension m n)&&
                     (check_dim m (List.length (List.hd m)))&&
                     (check_dim n (List.length (List.hd n))) then
                     SMatrix(scalar_matrices plus m n)
                  else failwith "Not correct dimensions"
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
                | Some e -> e::t
                | None -> h::(times_help t exp)
                )

and times (e1,e2) = 
  match e1,e2 with
  | SFloat a, STimes(c,l) -> unbox(STimes (c*.a,l))
  | SFloat f, SMatrix m   -> if check_dim m (List.length (List.hd m)) then
                           (let fmatrix = create_matrix (List.length m)
                           (List.length (List.hd m)) (SFloat f) in
                             SMatrix(scalar_matrices times m fmatrix))
                           else
                             failwith "Not correct dimensions"
  | SMatrix m, SFloat f   -> times (SFloat f, SMatrix m)
  | STimes(c,l), SFloat a -> unbox(STimes (c*.a,l))
  | SFloat a, SFloat b    -> SFloat(a*.b)
  | SFloat a, e           -> unbox(match compare_mult e1 e2 with Some e -> e | None -> STimes (a,[e]))
  | e, SFloat a           -> unbox(match compare_mult e1 e2 with Some e -> e | None -> STimes (a,[e]))
  | STimes (c1,l1), STimes (c2,l2)  -> unbox(STimes (c1*.c2,List.fold_left times_help l1 l2))
  | STimes (c,l), _         -> unbox(STimes (c,times_help l e2))
  | _, STimes (c,l)         -> unbox(STimes (c,times_help l e1))
  | s1, s2                -> unbox(match compare_mult s1 s2 with Some e -> e | None -> STimes (1.,[s1;s2]))

let s_times l = unbox(List.fold_left (fun a b -> times (a,b)) (STimes (1.,[])) l)
let s_plus l = unbox(List.fold_left (fun a b -> plus (a,b)) (SPlus []) l)

let rec remove_at n = function
  | [] -> []
  | h::t -> if n = 0 then t else h::remove_at (n-1) t

(* Matrix multiplication on Matrix m with nxm and Matrix n with pxq, m must = p*)
let matrix_mult m n =
  let x0 = List.length m in
  let y0 = List.length n in
  let x1 = (if x0 > 0 then List.length (List.hd m) else failwith "Not correct dimensions") in
  if x1 <> y0 then failwith "Not correct dimensions"
  else
    let tn = trans_matrix n in
    let rec helper mat tnat =
    (match mat with
    | [] -> []
    | h::t -> let rec helper2 ma tna =
              (match tna with
                | [] -> []
                | s::e -> s_plus(List.hd (scalar_matrices times [ma] [s]))::helper2 ma e)
              in helper2 h (tnat)::helper t tnat) in helper m tn

(* Removes the ith column and the first row in a matrix (for determinants)*)
let remove m i =
    let newm = List.tl m in
    List.map (remove_at i) newm

let prep_det m =
  match m with
  | [] -> []
  | h::t -> let rec helper i n =
            (match n with
            | [] -> []
            | s::e -> if i = 1 then times(s,SFloat(-1.))::helper (i-1) e
              else s::helper (i+1) e) in
              (helper 0 h)::t

(* Returns the determinant of a matrix*)
let determinant m =
  let row = List.length m in
  let _ = for i = 0 to (row-1) do
            if (List.length (List.nth m i)) <> row
              then failwith "Matrix not square"
            else ()
          done in
  let rec helper hm =
    let acc = ref [] in
    (match hm with
    | [] -> SFloat 1.
    | h::t -> if List.length h = 2 then
                plus (times(List.hd h, List.nth (List.hd t) 1),times(times(List.hd (List.hd t), List.hd(List.tl h)),SFloat(-1.)))
              else
                let newh = List.hd (prep_det hm) in
                let () = (for i = 0 to (List.length newh) - 1 do
                  acc := times((List.nth newh i),helper (remove hm i))::!acc
                done) in List.fold_left (fun accum x -> plus(accum,x)) (SFloat 0.) !acc
                  )
    in helper m
    
(*[deriv s1 s2] returns the derivative of s1 with respect to s2*)

(*NOTE: For future, use the functions s_times, s_plus, and pow instead of STimes, SPlus, and SPow when constructing new
lists of expressions. pow can directly replace SPow. s_times can directly replace STimes, and s_plus can directly replace SPlus
*)

let rec deriv s1 s2 =
  match s1, s2 with
  | SFloat _, _ | SPI, SVar _ | SE, SVar _ -> SFloat 0.
  | SVar x, SVar x'       -> if (x=x') then (SFloat 1.) else SFloat 0.
  | STimes (c,[h]), SVar _    -> times(SFloat c, deriv h s2)
  | STimes (c,h::t), SVar _ -> let l1 = s_times [(deriv h s2);STimes (c,t)] in
                              let l2 = s_times [h;deriv (STimes (c,t)) s2] in
                              s_plus [l1; l2]
  | SPlus [h], SVar _     -> deriv h s2
  | SPlus (h::t), SVar _  -> s_plus [deriv h s2; deriv (SPlus t) s2]
  | SPow (f, g), SVar _   -> times(pow(f,g),plus(times(deriv g s2, SLog(f)), 
                               s_times[g; (deriv f s2); pow(f, SFloat (-1.))]))
  | SMatrix x, SVar _     -> failwith "TODO"
  | SSin x, SVar _        -> s_times [SCos x; deriv x s2] 
  | SCos x, SVar _        ->  s_times [SFloat (-1.); SSin x; deriv x s2]
  | SLog x, SVar _        ->  s_times [pow (x, SFloat (-1.)); deriv x s2]
  | _, _                  -> failwith "This shouldn't happen"


let is_constant s = true (*JUST A PLACEHOLDER. STILL NEEDS TO BE DONE*)

let rec by_parts u dv s2 =
                        let du = deriv u s2 in let v = integrate dv s2 in
                        plus(times(u,v),integrate (s_times[SFloat (-1.);v; du]) s2)

and integrate s1 s2 = 
 match s1, s2 with
 | SFloat f, SVar x -> times(SFloat f, s2)
 | SVar x, SVar x' -> if (x=x') then (times(SFloat (1./.2.), pow(s1, SFloat 2.)))
                      else times(s1, s2) 
 | STimes (c,[h]), SVar _   -> times(SFloat c, integrate h s2)
 | STimes (c,(SSin x)::t), SVar _ -> by_parts (STimes(c,t)) (SSin x) s2
 | STimes (c,(SCos x)::t), SVar _ -> by_parts (STimes(c,t)) (SCos x) s2
 | STimes (c,h::t), SVar _ -> by_parts h (STimes(c,t)) s2
 | SPlus [h], SVar _     -> integrate h s2
 | SPlus (h::t), SVar _  -> s_plus [integrate h s2; integrate (SPlus t) s2]
 | SPow (SVar x, SFloat (-1.)), SVar x' when x = x' -> SLog(SVar x)
 | SPow (SVar x, g), SVar x' when x = x' && is_constant g ->  times(pow(SVar x, plus(SFloat 1., g)),pow(plus(SFloat 1., g), SFloat (-1.)))
 | SPow (SE, SVar x), SVar x' when x = x' -> pow(SE, SVar x)
 | SPow (f, g), SVar _   -> failwith "TODO"
 | SMatrix x, SVar _     -> failwith "TODO"
 | SSin x, SVar x'        -> (match x with 
                            | SFloat f -> times(SSin x, SVar x')
                            | SVar v when v = x'-> SCos x
                            | SVar v when v <> x'-> times(SSin x, SVar x')
                            | _ -> failwith "TODO")
 | SCos x, SVar x'        -> (match x with 
                            | SFloat f -> times(SCos x, SVar x')
                            | SVar v when v = x' -> times(SFloat (-1.), SSin x)
                            | SVar v when v <> x' -> times(SCos x, SVar x')
                            | _ -> failwith "TODO")
 | SLog x, SVar x'       -> (match x with 
                            | SFloat f -> times(SLog x, SVar x')
                            | SVar v when v = x' -> plus(times(x, SLog x), times(SFloat (-1.), x))
                            | SVar v when v <> x' -> times(SLog x, SVar x')
                            | _ -> failwith "TODO")
 | SPI, SVar _           -> times(SPI, s2)
 | SE, SVar _            -> times(SE, s2)
 | _, _                  -> failwith "This shouldn't happen"



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
  | SMatrix _ -> failwith "TODO"
  | SSin x -> SSin (subst (k,v) x)
  | SCos x -> SCos (subst (k,v) x)
  | SLog x -> SLog (subst (k,v) x)
  | SE -> SE
  | SPI -> SPI

let bin_op op s1 s2 =
    match op with
    | Plus   -> plus (s1, s2)
    | Times  -> times (s1, s2)
    | Minus  -> plus (s1, times(s2, SFloat (-1.)))
    | Pow    -> pow (s1, s2)
    | Divide -> times (s1, pow(s2, SFloat (-1.)))
    | Deriv  -> deriv s1 s2
    | Integrate -> integrate s1 s2

let un_op op s =
    match op with
    | Neg       -> times (SFloat (-1.), s)
    | Sin       -> SSin s
    | Cos       -> SCos s
    | Log       -> SLog s
    | Trans     -> (match s with
                   | SMatrix m -> if check_dim m (List.length (List.hd m)) then
                                    SMatrix(trans_matrix m)
                                  else failwith "Not correct dimensions"
                   | _ -> failwith "Can't take the transpose of non-matrices")
    | Det       -> (match s with SMatrix m -> determinant m | _ ->
                            failwith "Can't take the determinant of a non-matrix")
    | Inv       -> failwith "TODO"
    | EigVector -> failwith "TODO"
    | EigValue  -> failwith "TODO"
    | RRef      -> failwith "TODO"

let rec eval = function
    | Float  f            -> SFloat f
    | Var    v            -> SVar v
    | BinOp  (op, e1, e2) -> bin_op op (eval e1) (eval e2)
    | UnOp   (op, e)      -> un_op op (eval e)
    | Matrix m            -> let rec helper m =
                             (match m with
                             | [] -> []
                             | h::t -> let rec helper1 m1 =
                                       (match m1 with
                                        | [] -> []
                                        | s::e -> (eval s)::helper1 e) in 
                                        helper1 h::helper t) in 
                                        SMatrix(helper m)
    | Subst  (Float f,Var v,e)      -> subst (v,f) (eval e)
    | Subst (Var v, Float f, e)     -> subst (v,f) (eval e)
    | Subst (_)           -> failwith "Cannot substitute that"
    | E                   -> SE
    | PI                  -> SPI

