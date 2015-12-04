
let rec gcd = function
  | 0, b            -> b
  | a, 0            -> a
  | a, b when a < b -> gcd(a, b mod a)
  | a, b            -> gcd(b, a mod b)

let non_rep_frac a =
  let multiplier = if a < 0. then -1 else 1 in
  let a = abs_float a in
  let precision = 100000000. in
  let a_int = int_of_float a in
  let a_dec = ((floor a) -. (abs_float a))*.(-1.) in
  let a_prec    = int_of_float( abs_float(a_dec*.precision)) in
  let prec_int = int_of_float precision in
  let gcd' = gcd(a_prec, prec_int) in
  (multiplier*(a_int*(prec_int/gcd') + a_prec/gcd'), prec_int/gcd')

let is_int n = n -. floor n < 0.000001 || ceil n -. n < 0.000001 

let rec find_loop_end n v =
  match n with
  | 6. -> None
  | n when is_int (((10.**n)*.v)-.v) -> Some n
  | n -> find_loop_end (n+.1.) v

let rec find_loop n v =
  match n, find_loop_end 1. ((10.**n)*.v) with
  | 6., _ -> None
  | _, None -> find_loop (n+.1.) v
  | _, Some n' -> Some (n,n'+.n)

let not_long a = is_int ((10.**6.)*.a)

let round x = 
  match abs(int_of_float (10.*.((floor x) -. x))) with
  | n when n >= 5 -> int_of_float (ceil x)
  | n             -> int_of_float x

let rep_frac (i,f) a = 
  let lhs = round (10.**f -. 10.**i) in
  let rhs = round ((10.**f)*.a -. (10.**i)*.a) in
  let gcd' = gcd (lhs, rhs) in
  (rhs/gcd', lhs/gcd')


let frac a = 
  match find_loop 0. a with
  | None -> non_rep_frac a
  | Some _ when not_long a -> non_rep_frac a
  | Some (x,y) -> rep_frac (x,y) a