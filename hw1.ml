(* 1.1 ============================================= *)
let rec pow x n =
  match n with
  | 0 -> 1
  | n -> x * pow x (n - 1);;

(* pow ~-3 3;; (* int: -27 *) *)

let rec float_pow x n = 
  match n with
  | 0 -> 1.0
  | n -> x *. float_pow x (n - 1);;

(* float_pow 1.5 2;; (* float: 2.25 *) *)

(* 1.2 ============================================= *)
let rec compress lst =
  match lst with
  | [] -> []  (* Only run if list is empty, also for exhausting pattern match possibilities *)
  | [n] -> [n]
  | h::t -> if h = List.hd t then compress t else h::compress t;;

(* compress ["a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];; *)

(* 1.3 ============================================= *)
let rec remove_if lst pred = 
  match lst with
  | [] -> []
  | h::t -> if pred h then remove_if t pred else h::remove_if t pred;;

(* remove_if [1;2;3;4;5] (fun x -> x mod 2 = 1);; *)

(* 1.4 ============================================= *)
let min (i: int) (j: int) = 
  if (i < j) then i else j;;

let rec sliceHelp lst i j idx =
  match lst with
  | [] -> []
  | h::t -> if (idx >= i && idx < j) then h::sliceHelp t i j (idx + 1) 
  else if idx >= j then [] (* Stop condition *)
  else sliceHelp t i j (idx + 1);;

let slice lst i j = (* Serves a double purpose of being a wrapper function and handling indices "gracefully" *)
  if (i < j) then sliceHelp lst i (min j (List.length lst)) 0
  else [];;

(* slice ["a";"b";"c";"d";"e";"f";"g";"h"] 2 6;; *)
(* slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 2;; *)
(* slice ["a";"b";"c";"d";"e";"f";"g";"h"] 3 20;; *)

(* 1.5 ============================================= *)
let rec updateClasses fn el classes =
  match classes with (* `a list list *)
  | []::t -> []  (* Shouldn't run? Just to exhaust possibilities *)
  | (h::[])::t ->   if (fn h el) then (h::[el])::t
                               else [h]::(updateClasses fn el t)
  | (h::more)::t -> if (fn h el) then ((h::more)@[el])::t
                               else (h::more)::(updateClasses fn el t)
  | [] -> [[el]];; (* End condition - create new class*)

let rec equivsHelp fn lst classes = 
  match lst with 
  | [] -> classes 
  | h::t -> equivsHelp fn t (updateClasses fn h classes);; (* 2::[3] *)

let equivs fn lst = 
  match lst with 
  | [] -> [[]]
  | h::t -> equivsHelp fn t [[h]];; (* 1:[2; 3] *)

(* equivs (fun x y -> (=) (x mod 3) (y mod 3)) [1; 2; 3; 4; 5; 6; 7; 8];; *)

(* 1.6 ============================================= *)
