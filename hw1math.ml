type expr = 
  | Const of int
  | Var of string
  | Plus of args
  | Mult of args
  | Minus of args
  | Div of args
and args = {arg1: expr; arg2: expr};;

(* Plus{arg1 = Mult{arg1 = Const 2; arg2 = Var "x"}; 
     arg2 = Mult{arg1 = Const 3; arg2 = Minus{arg1 = Var "y"; arg2 = Const 1}}};; *)

let rec evaluate exp = 
  match exp with
  | Const n -> n
  | Var x -> 0
  | Plus {arg1; arg2} -> evaluate arg1 + evaluate arg2
  | Mult {arg1; arg2} -> evaluate arg1 * evaluate arg2
  | Minus {arg1; arg2} -> evaluate arg1 - evaluate arg2
  | Div {arg1; arg2} -> evaluate arg1 / evaluate arg2;;

(* let exp = Plus{arg1 = Mult{arg1 = Const 2; arg2 = Const 3}; 
               arg2 = Mult{arg1 = Const 3; arg2 = Minus{arg1 = Const 4; arg2 = Const 1}}} in evaluate(exp);; *)