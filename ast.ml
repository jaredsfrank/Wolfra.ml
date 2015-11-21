type var = string
type constant = string


type binop = 
    | Plus | Times | Minus | Divide | Pow | Deriv


type unop = 
    | Neg | Sin | Cos | Log
    | Trans | Inv | EigVector | EigValue | RRef 


type expr =                              
  | Float    of float 
  | Var      of var
  | BinOp    of binop * expr * expr
  | UnOp     of unop * expr
  | Matrix   of expr list list
  | PI
  | E

