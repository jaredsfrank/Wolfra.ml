type var = string
type constant = string


type binop = 
    | Plus | Times | Minus | Divide | Pow | Deriv | Integrate


type unop = 
    | Neg | Sin | Cos | Tan | Log |
    | Trans | Det | Inv | EigVector | EigValue | RRef 


type expr =                              
  | Float    of float 
  | Var      of var
  | BinOp    of binop * expr * expr
  | UnOp     of unop * expr
  | Matrix   of expr list list
  | Subst    of expr * expr * expr
  | PI
  | E

