open Simplify

(*[deriv s1 s2] returns the derivative of s1 with respect to s2*)

(*NOTE: For future, use the functions s_times, s_plus, and pow instead of STimes, SPlus, and SPow when constructing new
lists of expressions. pow can directly replace SPow. s_times can directly replace STimes, and s_plus can directly replace SPlus
*)

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
  | SPow (f, g), SVar _     -> times(pow(f,g),plus(times(deriv g s2, SLog(f)),
                               s_times[g; (deriv f s2); pow(f, SFloat (-1.))]))
  | SMatrix m, SVar _       -> SMatrix (List.map (fun l -> List.map
                               (fun x -> deriv x s2) l) m)
  | SSin x, SVar _          -> s_times [SCos x; deriv x s2]
  | SCos x, SVar _          ->  s_times [SFloat (-1.); SSin x; deriv x s2]
  | SLog x, SVar _          ->  s_times [pow (x, SFloat (-1.)); deriv x s2]
  | _, _                    -> failwith "This shouldn't happen"
