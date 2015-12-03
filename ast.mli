(*calc variable names i.e x when deriving f(x)*)
type var = string

(*variable names that represent constants i.e c, k, etc*)
type constant = string

(*binary operations*)
type binop = 
    | Plus | Times | Minus | Divide | Pow | Deriv | Integrate | Ass

(*unary operations*)
type unop = 
    | Neg | Sin | Cos | Tan | Log
    | Trans | Det | Inv | EigVector | EigValue | RRef 

(*Type of mathematical expressions
 * Float 8.5 represents 8.5
 * Var "x" represents a variable x that is involved in calculus
 * BinOp (Plus, e1, e2) represents e1 + e2
 * Unop (Deriv, e1) represents the symbolic derivative of e1
 * Matrix ([[1;2;3];[4;5;6]]) represents the associated 2-row matrix
 * PI represents the number pi
 * E represents the number e
*)
type expr =                              
  | Float    of float 
  | Var      of var
  | BinOp    of binop * expr * expr
  | UnOp     of unop * expr
  | Matrix   of expr list list
  | Subst    of expr * expr * expr
  | PI
  | E
  | Ans
