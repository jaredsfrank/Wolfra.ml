open Simplify
open Derivative

let is_constant s = true (*JUST A PLACEHOLDER. STILL NEEDS TO BE DONE*)

let rec integrate s1 s2 =
 match s1, s2 with
 | SFloat f, SVar x -> times(SFloat f, s2)
 | SVar x, SVar x' -> if (x=x') then (times(SFloat 0.5, pow(s1, SFloat 2.)))
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
 | SPow (SE, STimes(c,[SVar x])), SVar x' when x = x' ->times(SFloat (1./.c), SPow(SE, STimes(c,[SVar x])))
 | SPow (SVar f, SFloat g), SVar x' when f = x' -> times(pow(plus(SFloat g, SFloat 1.), SFloat (-1.)),pow(SVar f,plus(SFloat g, SFloat 1.)))
 | SPow (SVar f, SFloat g), SVar x' when f <> x' -> times(SPow (SVar f, SFloat g), SVar x')
 | SPow ( SPlus( STimes(c1,[SVar v]) :: [SFloat c2] ), SFloat (-1.)), SVar v' when v'=v -> times(pow(SFloat c1, SFloat (-1.)), SLog(SPlus( STimes(c1,[SVar v]) :: [SFloat c2] )))
 | SPow (SPlus (SVar x::[SFloat f]), SFloat (-1.)), SVar v when v = x -> SLog(SPlus (SVar x::[SFloat f]))
 | SMatrix m, SVar _     -> SMatrix (List.map (fun l -> List.map (fun x -> integrate x s2) l) m)
 | SSin x, SVar x'        -> (match x with
                            | SFloat f -> times(SSin x, SVar x')
                            | SPI | SE -> times(SSin x, SVar x')
                            | SVar v when v = x'-> times(SFloat (-1.),SCos x)
                            | SVar v when v <> x'-> times(SSin x, SVar x')
                            | STimes(f, [SVar v]) when v=x' -> times(pow(times(SFloat (-1.), SFloat f), SFloat (-1.)), SCos(x))
                            | STimes(f, [SVar v]) when v<>x' -> times(SSin x, SVar x')
                            | STimes(f, SVar v1::[SVar v2]) when v1=x' -> times(pow(s_times(SFloat f::[SVar v2]),SFloat (-1.)),times(SFloat (-1.),SCos x))
                            | STimes(f, SVar v1::[SVar v2]) when v2=x' -> times(pow(s_times(SFloat f::[SVar v1]),SFloat (-1.)),times(SFloat (-1.),SCos x))
                            | SPlus(SVar v::[SFloat f]) | SPlus(SFloat f::[SVar v]) when v = x'-> times(SFloat (-1.), SCos(SPlus(SVar v::[SFloat f])))
                            | SPlus(SVar v::[SFloat f]) | SPlus(SFloat f::[SVar v]) when v <> x'-> times(SSin x, SVar x')
                            | _ -> failwith "TODO")
 | SCos x, SVar x'        -> (match x with
                            | SFloat f -> times(SCos x, SVar x')
                            | SPI | SE -> times(SCos x, SVar x')
                            | SVar v when v = x' -> SSin x
                            | SVar v when v <> x' -> times(SCos x, SVar x')
                            | STimes(f, [SVar v]) when v=x' -> times(pow(SFloat f, SFloat (-1.)), SSin(x))
                            | STimes(f, [SVar v]) when v<>x' -> times(SCos x, SVar x')
                            | STimes(f, SVar v1::[SVar v2]) when v1=x' -> times(pow(s_times(SFloat f::[SVar v2]),SFloat (-1.)),SSin x)
                            | STimes(f, SVar v1::[SVar v2]) when v2=x' -> times(pow(s_times(SFloat f::[SVar v1]),SFloat (-1.)),SSin x)
                            | SPlus(SVar v::[SFloat f]) | SPlus(SFloat f::[SVar v]) when v = x'-> SSin(SPlus(SVar v::[SFloat f]))
                            | SPlus(SVar v::[SFloat f]) | SPlus(SFloat f::[SVar v]) when v <> x'-> times(SCos x, SVar x')
                            | _ -> failwith "TODO")
 | SLog x, SVar x'       -> (match x with
                            | SFloat f -> times(SLog x, SVar x')
                            | SPI -> times(SLog x, SVar x')
                            | SVar v when v = x' -> plus(times(x, SLog x), times(SFloat (-1.), x))
                            | SVar v when v <> x' -> times(SLog x, SVar x')
                            | STimes(f, [SVar v]) when v=x' -> plus(times(SVar x', SLog x), times(SFloat (-1.), SVar x'))
                            | STimes(f, [SVar v]) when v<>x' -> times(SLog x, SVar x')
                            | SPlus(SVar v::[SFloat f]) | SPlus(SFloat f::[SVar v]) when v = x' -> plus(times(x, SLog x), times(SFloat (-1.),SVar x'))
                            | _ -> failwith "TODO")
 | SPI, SVar _           -> times(SPI, s2)
 | SE, SVar _            -> times(SE, s2)
 | _, _                  -> failwith "This shouldn't happen"

and by_parts u dv s2 =
    let du = fst (deriv u s2) in let v = integrate dv s2 in
    plus(times(u,v),integrate (s_times[SFloat (-1.);v; du]) s2)


