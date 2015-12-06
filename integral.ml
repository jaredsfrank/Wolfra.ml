open Simplify
open Derivative

let rec is_constant x' = function
    | SFloat f -> true
    | SVar x when x = x' -> false
    | SVar x  -> true
    | STimes (c, l) -> List.fold_left 
                      (fun accum a -> accum && is_constant x' a) true l
    | SPlus l       -> List.fold_left 
                      (fun accum a -> accum && is_constant x' a) true l
    | SPow (e1, e2) -> is_constant x' e1 && is_constant x' e2
    | SMatrix l     -> List.fold_left (fun accum l' -> accum && 
                       List.fold_left 
                     (fun accum a -> accum && is_constant x' a) true l') true l
    | SSin e -> is_constant x' e
    | SCos e -> is_constant x' e
    | SLog e -> is_constant x' e
    | SE -> true
    | SPI -> true

let sin_integration x x' =
 match x with
 | SFloat f -> times(sin_function x, SVar x')
 | SPI | SE -> times(sin_function x, SVar x')
 | SVar v when v = x'-> times(SFloat (-1.),cos_function x)
 | SVar v when v <> x'-> times(sin_function x, SVar x')
 | STimes(f, [SVar v]) when v=x' ->divide(cos_function(x),times(SFloat (-1.), SFloat f))
 | STimes(f, [SVar v]) when v<>x' -> times(sin_function x, SVar x')
 | STimes(f, SVar v1::[SVar v2]) when v1=x' ->
                           times(pow(s_times(SFloat f::[SVar v2]),SFloat (-1.)),
                           times(SFloat (-1.),cos_function x))
 | STimes(f, SVar v1::[SVar v2]) when v2=x' ->
                           times(pow(s_times(SFloat f::[SVar v1]),SFloat (-1.)),
                           times(SFloat (-1.),cos_function x))
 | SPlus(SVar v::[SFloat f])
 | SPlus(SFloat f::[SVar v]) when v = x'-> times(SFloat (-1.),
                                           cos_function(SPlus(SVar v::[SFloat f])))
 | SPlus(SVar v::[SFloat f])
 | SPlus(SFloat f::[SVar v]) when v <> x'-> times(sin_function x, SVar x')
 | _ -> failwith "Error: Wolfra.ml does not support that type of integration"

let cos_integration x x' =
 match x with
 | SFloat f -> times(cos_function x, SVar x')
 | SPI | SE -> times(cos_function x, SVar x')
 | SVar v when v = x' -> sin_function x
 | SVar v when v <> x' -> times(cos_function x, SVar x')
 | STimes(f, [SVar v]) when v=x' -> divide(sin_function x, SFloat f)
 | STimes(f, [SVar v]) when v<>x' -> times(cos_function x, SVar x')
 | STimes(f, SVar v1::[SVar v2]) when v1=x' -> divide(sin_function x,
                                               s_times(SFloat f::[SVar v2]))
 | STimes(f, SVar v1::[SVar v2]) when v2=x' -> divide(sin_function x,
                                               s_times(SFloat f::[SVar v1]))
 | SPlus(SVar v::[SFloat f])
 | SPlus(SFloat f::[SVar v]) when v = x'-> sin_function(SPlus(SVar v::[SFloat f]))
 | SPlus(SVar v::[SFloat f])
 | SPlus(SFloat f::[SVar v]) when v <> x'-> times(cos_function x, SVar x')
 | _ -> failwith "Error: Wolfra.ml does not support that type of integration"

let log_integration x x' =
 match x with
 | SFloat f -> times(log_function x, SVar x')
 | SPI -> times(log_function x, SVar x')
 | SVar v when v = x' -> plus(times(x, log_function x), times(SFloat (-1.), x))
 | SVar v when v <> x' -> times(log_function x, SVar x')
 | STimes(f, [SVar v]) when v=x' -> plus(times(SVar x', log_function x),
                                    times(SFloat (-1.), SVar x'))
 | STimes(f, [SVar v]) when v<>x' -> times(log_function x, SVar x')
 | SPlus(SVar v::[SFloat f])
 | SPlus(SFloat f::[SVar v]) when v = x' -> plus(times(x, log_function x),
                                            times(SFloat (-1.),SVar x'))
 | _ -> failwith "Error: Wolfra.ml does not support that type of integration"

let rec integrate s1 s2 =
 match s1, s2 with
 | SFloat f, SVar x -> times(SFloat f, s2)
 | SVar x, SVar x' -> if (x=x') then (times(SFloat 0.5, pow(s1, SFloat 2.)))
                      else times(s1, s2)
 | STimes (c,[h]), SVar _   -> times(SFloat c, integrate h s2)
 | STimes (c,(SSin x)::t), SVar _ -> by_parts (STimes(c,t)) (sin_function x) s2
 | STimes (c,(SCos x)::t), SVar _ -> by_parts (STimes(c,t)) (cos_function x) s2
 | STimes (c,(SPow(SE, x))::t), SVar _ ->by_parts (STimes(c,t)) (SPow(SE, x)) s2
 | STimes (c,h::t), SVar _ -> by_parts h (STimes(c,t)) s2
 | SPlus [h], SVar _     -> integrate h s2
 | SPlus (h::t), SVar _  -> s_plus [integrate h s2; integrate (SPlus t) s2]
 | SPow (SVar x, SFloat (-1.)), SVar x' when x = x' -> log_function(SVar x)
 | SPow (SVar x, g), SVar x'
    when x = x' && is_constant x' g -> divide(pow(SVar x, plus(SFloat 1., g)),
                                    plus(SFloat 1., g))
 | SPow (SVar x, g), SVar x' when is_constant x' g -> times(s1, SVar x')
 | SPow (SE, SVar x), SVar x' when x = x' -> pow(SE, SVar x)
 | SPow (SE, STimes(c,[SVar x])), SVar x'
    when x = x' ->times(SFloat (1./.c), SPow(SE, STimes(c,[SVar x])))
 | SPow (SPlus(STimes(c1,[SVar v]) :: [SFloat c2]), SFloat (-1.)), SVar v'
    when v'=v -> divide(log_function(SPlus( STimes(c1,[SVar v])::[SFloat c2] )),
                 SFloat c1)
 | SPow (SPlus (SVar x::[SFloat f]), SFloat (-1.)), SVar v
    when v = x -> log_function(SPlus (SVar x::[SFloat f]))
 | SMatrix m, SVar _     -> SMatrix (List.map (fun l -> List.map
                            (fun x -> integrate x s2) l) m)
 | SSin x, SVar x'       -> sin_integration x x'
 | SCos x, SVar x'       -> cos_integration x x'
 | SLog x, SVar x'       -> log_integration x x'
 | SPI, SVar _           -> times(SPI, s2)
 | SE, SVar _            -> times(SE, s2)
 | _, SVar _             -> failwith ("Error: Wolfra.ml does not support that"^
                            " type of integration")
 | _, _                  -> failwith ("Error: Please Integrate with respect "^
                            " to a variable")
and by_parts u dv s2 =
    let du = deriv u s2 in let v = integrate dv s2 in
    plus(times(u,v),integrate (s_times[SFloat (-1.);v; du]) s2)


