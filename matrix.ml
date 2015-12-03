open Simplify


(* Checks two matrices' lists to make sure their dimensions are the same*)
let rec check_dimension =
    List.fold_left2 (fun accum a b -> accum && List.length a = List.length b) true


let is_square m = 
  List.fold_left (fun b l -> (List.length l = List.length m) && b) true m

let is_rect m =
  match m with
  | [] -> true
  | h::t -> List.fold_left (fun b l -> (List.length l = List.length h) && b) true m


(*Creates the s_expr list list of dimensions row col with expression f in every position*)
let create_matrix row col f =
    let rec make_rows c acc = if c = 0 then acc else make_rows (c-1) (f::acc) in
    let rows = make_rows col [] in
    let rec combine_rows r acc = if r = 0 then acc else combine_rows (r-1) (rows::acc) in
    combine_rows row []

(*Creates the identity matrix of size nxn, matrix should be an empty matrix*)
let rec identity n m matrix =
  let rec helper l n m =
  (match l with
  | [] -> []
  | h::t -> if n = m then (SFloat 1.)::t else h::helper t (n+1) m) in
  match matrix with
  | [] -> []
  | h::t -> (helper h 0 m)::(identity n (m+1) t)
  

let rec remove_at n = function
  | [] -> []
  | h::t -> if n = 0 then t else h::remove_at (n-1) t


(* Removes the ith column and the first row in a matrix (for determinants)*)
let remove m i =
    let newm = List.tl m in
    List.map (remove_at i) newm

(* Returns the determinant of a matrix*)
let rec determinant m =
  let rec helper i = function
    | [] -> SFloat 0.
    | h::t when i mod 2 = 0-> plus(times (h, determinant (remove m i)),(helper (i+1) t))
    | h::t -> plus(s_times [SFloat (-1.); h; determinant (remove m i)],(helper (i+1) t)) in
  match m with
    | [] -> SFloat 1.
    | h::t -> helper 0 h 

(* Returns the transpose of a matrix*)
let rec trans_matrix = function
  | [] -> []
  | []::t -> trans_matrix t
  | (h::t1)::t2 -> (h::(List.map List.hd t2))::trans_matrix (t1::(List.map List.tl t2))


let dot l1 l2 = List.fold_left2 (fun accum a b -> plus(accum,times (a,b))) (SFloat 0.) l1 l2 

let dot_matrices m1 m2 = 
  try List.map (fun l -> List.map (dot l) m1) m2
  with _ -> failwith "Err Matrix Mult"


let matrix_times (e1,e2) = 
  match (e1, e2) with
  | SFloat _, SMatrix m    -> SMatrix (List.map (fun l -> List.map (fun x -> times (x,e1)) l) m)
  | SMatrix m, SFloat _    -> times (e2, SMatrix m)
  | SMatrix m1, SMatrix m2 -> SMatrix (dot_matrices m1 (trans_matrix m2))
  | _, _                   -> failwith "uh oh"

and matrix_plus = function
  | SMatrix m, SFloat f -> SMatrix (List.map (fun l -> List.map (fun x -> plus (x,SFloat f)) l) m)
  | SFloat f, SMatrix m -> plus (SMatrix m, SFloat f)
  | SMatrix m, SMatrix n when check_dimension m n->  SMatrix(List.map2 (fun l1 l2 -> List.map2 (fun a b -> plus(a,b)) l1 l2) m n)
  | _,_ -> failwith "uh oh"

(*Swaps rows i and j of a matrix. if i=j then the matrix stays the same*)
let swap_rows m i j =
  let ne = ref [] in
  let _ = for c = 0 to ((List.length m)-1) do
    if c = (i) then ne:=!ne@[List.nth m j]
    else
      if c = (j) then ne:=!ne@[List.nth m i]
      else
        ne:=!ne@[List.nth m c]
  done in !ne
(*Returns the row-reduced echelon form of a matrix, derived from the array
 * solution on http://rosettacode.org/wiki/Reduced_row_echelon_form#OCaml*)
let rref m =
  let n = ref m in
  try
    let lead = ref 0
    and rows = List.length !n
    and cols = List.length (List.hd !n) in
    let _ = for r = 0 to pred rows do
      if cols <= !lead then
        raise Exit;
      let i = ref r in
      while (List.nth (List.nth !n !i) !lead) = (SFloat 0.) do
        let _ = incr i in
        if rows = !i then begin
          i := r;
          incr lead;
          if cols = !lead then
            raise Exit;
          end
      done;
      n:=swap_rows !n !i r;
      let lv = (List.nth (List.nth !n r) !lead) in
      let mr = List.map (fun v -> times(v, pow(lv,SFloat (-1.)))) (List.nth !n r) in
      n:=[mr]@(!n);
      n:= swap_rows !n 0 (r+1);
      n:= List.tl !n;
      for i = 0 to pred rows do
        if i <> r then
          let lv = (List.nth (List.nth !n i) !lead) in
          let mi = List.mapi (fun i iv -> plus(iv,times(times(lv,SFloat (-1.)),(List.nth (List.nth !n r) i)))) (List.nth !n i) in
          n:= [mi]@(!n);
          n:= swap_rows !n 0 (i+1);
          n:= List.tl !n;
      done;
      incr lead;
    done in !n
  with Exit -> !n

(*Derives the inverse of a matrix from matrix m*)
let inv_matrix m =
    let rows = (List.length m) in
    let ident = identity rows 0 (create_matrix rows rows (SFloat 0.)) in
    let rec helper1 m n =
    (match m, n with
    | [], [] -> []
    | h1::t1, h2::t2 -> (h1@h2)::helper1 t1 t2
    | _, _ -> failwith "Err Square") in
    let reduced = rref(helper1 m ident) in
    let rec helper2 red n =
    (match red with
    | [] -> []
    | h::t -> if n >= rows then red else helper2 t (n+1)) in
    List.map (fun x -> helper2 x 0) reduced
    
let quadratic a b c =
  let discrim = plus(pow(b,SFloat 2.),times(SFloat (-1.),times(SFloat 4.,times(a,c)))) in
  match discrim with
  | SFloat i -> if i < 0. then failwith "No complex solutions"
                else (times(pow(times(a,SFloat 2.),SFloat (-1.)),plus(times(b,SFloat (-1.)),pow(discrim,SFloat (0.5)))) ,
                      times(pow(times(a,SFloat 2.),SFloat (-1.)),plus(times(b,SFloat (-1.)),times(SFloat (-1.),pow(discrim,SFloat (0.5))))))
  | _ -> failwith "This should not happen"

let eigenv m =
    let det = determinant m in
    quadratic (SFloat 1.)
    (times(SFloat (-1.),(plus(List.hd (List.hd m),List.nth (List.nth m 1) 1))))
    det
