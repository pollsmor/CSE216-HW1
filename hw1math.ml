type expr = 
  | Const of int
  | Var of string
  | Plus of args
  | Mult of args
  | Minus of args
  | Div of args
and args = {arg1: expr; arg2: expr};;

Plus{arg1 = Mult{arg1 = Const 2; arg2 = Var "x"}; 
     arg2 = Mult{arg1 = Const 3; arg2 = Minus{arg1 = Var "y"; arg2 = Const 1}}};;