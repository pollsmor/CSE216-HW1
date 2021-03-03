(* 1.1 ============================================= *)
let rec pow x n =
  match n with
  | 0 -> 1
  | n -> x * pow x (n - 1);;

pow ~-3 3;; (* int: -27 *)

let rec float_pow x n = 
  match n with
  | 0 -> 1.0
  | n -> x *. float_pow x (n - 1);;

  float_pow 1.5 2;; (* float: 2.25 *)

(* 1.2 ============================================= *)
let rec compress lst =
  match lst with
  | [] -> []  (* Only run if list is empty, also for exhausting pattern match possibilities *)
  | [n] -> [n]
  | h::t -> if h = List.hd t then compress t else h::compress t;;

compress ["a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];;