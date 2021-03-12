(* 2.1 ============================================= *)
type bool_expr =
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let rec eval2 lit1 (lit1val: bool) lit2 (lit2val: bool) expr = 
  match expr with
  | Lit x -> if x = lit1 then lit1val else lit2val
  | Not e -> not(eval2 lit1 lit1val lit2 lit2val e)
  | And(e1, e2) -> eval2 lit1 lit1val lit2 lit2val e1 && eval2 lit1 lit1val lit2 lit2val e2
  | Or(e1, e2) -> eval2 lit1 lit1val lit2 lit2val e1 || eval2 lit1 lit1val lit2 lit2val e2

let truth_table lit1 lit2 expr =
  [
    (true, true, eval2 lit1 true lit2 true expr);
    (true, false, eval2 lit1 true lit2 false expr);
    (false, true, eval2 lit1 false lit2 true expr);
    (false, false, eval2 lit1 false lit2 false expr)
  ];;

(* truth_table "a" "b" (Or(And(Lit("a"), Lit("b")), Not(Lit("a"))));; *)