open Simplify


let rec deriv s1 s2 =
  match s1, s2 with
  | SFloat _, _ | SPI, SVar _ | SE, SVar _ -> SFloat 0.
  | SVar x, SVar x'         -> if (x=x') then (SFloat 1.) else SFloat 0.
  | STimes (c,[h]), SVar _  -> times(SFloat c, deriv h s2)
  | STimes (c,h::t), SVar _ -> let l1 = s_times [(deriv h s2);STimes (c,t)] in
                              let l2 = s_times [h;deriv (STimes (c,t)) s2] in
                              s_plus [l1; l2]
  | SPlus [h], SVar _       -> deriv h s2
  | SPlus (h::t), SVar _    -> s_plus [deriv h s2; deriv (SPlus t) s2]
  | SPow (f, g), SVar _     -> times(pow(f,g),plus(times(deriv g s2, log_function(f)),
                               s_times[g; (deriv f s2); pow(f, SFloat (-1.))]))
  | SMatrix m, SVar _       -> SMatrix (List.map (fun l -> List.map
                               (fun x -> deriv x s2) l) m)
  | SSin x, SVar _          -> s_times [cos_function x; deriv x s2]
  | SCos x, SVar _          ->  s_times [SFloat (-1.); sin_function x; deriv x s2]
  | SLog x, SVar _          ->  s_times [pow (x, SFloat (-1.)); deriv x s2]
  | _, _                    -> failwith ("Error: Please derive with respect "^
                                        "to a variable")

let rec multi_deriv n s1 s2 =
  match n with
  | 0. -> s1
  | n -> multi_deriv (n-.1.) (deriv s1 s2) s2

let rec fact = function
  | 0. -> 1.
  | n -> n *. fact (n-.1.)