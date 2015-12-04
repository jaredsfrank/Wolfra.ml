open Simplify

(*This module system is inspired by A6 Monad section*)
module type Monad = sig
 type 'a t
 val bind : 'a t -> ('a -> 'b t) -> 'b t
 val return : 'a -> 'a t
end

module type   LogMonad = sig
  include Monad
  val log: string list -> unit t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
end

module Logger : (LogMonad with type 'a t = 'a * string list) =
  struct
    type 'a t = 'a * string list
    let bind (x, s1) f = let (y, s2) = f x in (y, s2@s1)
    let (>>=) = bind
    let return x = (x, [])
    let log s = ((), s)
  end

open Logger

let rec deriv s1 s2 =
  match s1, s2 with
  | SFloat _, _ | SPI, SVar _ | SE, SVar _ -> bind (log["Derivative of a constant is zero"]) 
                                              (fun () -> return (SFloat 0.))
  | SVar x, SVar x'         -> if (x=x') then return (SFloat 1.) else return (SFloat 0.)
  | STimes (c,[h]), SVar _  ->  (deriv h s2) >>= (fun x -> (times(SFloat c, x), ["Times"]))
  | STimes (c,h::t), SVar _ ->  deriv h s2 >>= fun x ->
                                bind (deriv (STimes (c,t)) s2) (fun y ->
                                (s_plus [ s_times [x;STimes (c,t)];s_times [h;y]], ["Times"]))
  | SPlus [h], SVar _       ->  deriv h s2
  | SPlus (h::t), SVar _    ->  deriv h s2 >>= fun x ->
                                deriv (SPlus t) s2 >>= fun y ->
                                (s_plus [x; y], ["Plus 2"])
  | SPow (f, g), SVar _     ->  deriv g s2 >>= fun dg ->
                                deriv f s2 >>= fun df ->
                                  (times(pow(f,g),plus(times(dg, SLog(f)),
                                 s_times[g; df; pow(f, SFloat (-1.))])), ["Pow"])
  | SMatrix m, SVar _       -> 
                                return (SMatrix  (List.map (fun l -> List.map
                                 (fun x -> fst (deriv x s2)) l) m))
  | SSin x, SVar _          ->   deriv x s2 >>= fun dx ->
                                (s_times [SCos x; dx], ["Sin"])
  | SCos x, SVar _          ->  deriv x s2 >>= fun dx ->
                                (s_times [SFloat (-1.); SSin x; dx], ["Cos"])
  | SLog x, SVar _          ->  deriv x s2 >>= fun dx ->
                                (s_times [pow (x, SFloat (-1.)); dx], ["Log"])
  | _, _                    -> failwith "This shouldn't happen"

let rec multi_deriv n s1 s2 =
  match n with
  | 0. -> return s1
  | n -> bind (deriv s1 s2) (fun x -> multi_deriv (n-.1.) x s2)

let rec fact = function
  | 0. -> 1.
  | n -> n *. fact (n-.1.)

let rec taylor n s1 s2 = 
  match n with
  | 0. -> SFloat 0.
  | n -> failwith "TODO"
        (*plus(s_times[multi_deriv n s1 s2;pow(plus(s2, times(SFloat (-1.), SVar "a")),SFloat n); SFloat (1./.fact n)], taylor (n-.1.) s1 s2)*)
