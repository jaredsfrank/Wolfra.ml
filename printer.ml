open Eval
let rec step_string _ = failwith "TODO"

let rec format_expr f e = 
    let bracket parent f e =
        Format.fprintf f "%a" format_expr e
    in
    match e with
    | SFloat n -> Format.fprintf f "@[%.2f@]" n
    | SVar x -> Format.fprintf f "@[%s@]" x
    | STimes [] -> Format.fprintf f "@[@]"
    | STimes [h] -> Format.fprintf f "@[%a@]" (bracket e) h
    | STimes (h::t) -> Format.fprintf f "@[%a*%a@]" (bracket e) h (bracket e) (STimes t)
    | SPlus [] -> Format.fprintf f "@[@]"
    | SPlus [h] -> Format.fprintf f "@[%a@]" (bracket e) h
    | SPlus (h::t) -> Format.fprintf f "@[%a+%a@]" (bracket e) h (bracket e) (SPlus t)
    | SPow (s1,s2) -> Format.fprintf f "@[(%a)^(%a)@]" (bracket e) s1 (bracket e) s2
    | SMatrix x ->  failwith "TODO"
    | SSin s ->  Format.fprintf f "@[sin(%a)@]" (bracket e) s
    | SCos s ->  Format.fprintf f "@[cos(%a)@]" (bracket e) s
    | SLog s ->  Format.fprintf f "@[ln(%a)@]" (bracket e) s
    | SE -> Format.fprintf f "@[e@]"
    | SPI  -> Format.fprintf f "@[pi@]"

let make_string_of f = Format.asprintf "%a" f
let string_of_expr     = make_string_of format_expr