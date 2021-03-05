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
let rec isPrime x curr = 
  if (x == 2 || x == 3) then true
  else if (curr = x/2 + 1) then true  (* I could go all the way to x - 1 but seems a waste *)
  else if (x mod curr == 0) then false  (* Modulo is 0 so curr must be a factor *)
  else isPrime x (curr + 1);;


let rec goldbachHelp x lo hi = 
  if (isPrime lo 2 && isPrime hi 2) then (lo, hi)
  else goldbachHelp x (lo - 1) (hi + 1);;


let goldbachpair x =
  (* Start looking from the center and expand outwards until a pair of primes is found *)
  if (x > 2 && x mod 2 = 0) then goldbachHelp x (x/2) (x/2) else (0, 0);;

(* goldbachpair 3181752;; *)

(* 1.7 ============================================= *)
let rec equiv_on f g lst =
  match lst with
  | [] -> true
  | h::t -> if (f h = g h) then equiv_on f g t else false;;

(* let f i = i * i;; *)
(* let g i = 3 * i;; *)
(* equiv_on f g [1;2;3];; *)

(* 1.8 ============================================= *)
let rec pairwisefilter cmp lst = 
  match lst with
  | [] -> []
  | h::[] -> [h] (* Lone element - return as is *)
  | h::t -> (cmp h (List.hd t))::pairwisefilter cmp (List.tl t);;
  (* List.tl t as I want to go 2 elements ahead *)

pairwisefilter min [14; 11; 20; 25; 10; 11];;

(* 1.9 ============================================= *)
let rec polynomialHelp args fxn = 
  match args with 
  | [] -> fxn
  (* snd is exponent, fst is coefficient *)
  (* Gist: add previous result to newer function's result *)
  | h::t -> polynomialHelp t (fun x -> fxn x + pow (x) (snd h) * (fst h));;

let polynomial args = 
  polynomialHelp args (fun x -> 0);;

(* let polyfxn = polynomial [3, 6; -2, 3; 5, 0];; *)
(* polyfxn 3;; *)

(* 1.10 ============================================= *)
(* Gist: append new element to existing power sets *)

let rec updateSets sets el =
  match sets with (* `a list list *)
  | []::t -> sets@[el]::(updateSets t el)
  | (h::[])::t -> (h::[el])::(updateSets t el)
  | (h::more)::t -> ((h::more)@[el])::(updateSets t el)
  | [] -> sets
  
let rec powersetHelp lst sets =
  match lst with 
  | [] -> sets
  | h::t -> powersetHelp t (updateSets sets h);;

let powerset lst = 
  powersetHelp lst [[]];;

powerset [3; 4; 10; 15];;