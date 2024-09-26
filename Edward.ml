(* Problem 6 Solution *)
(* Author: Edward Campion III *)
let rec makepairs x lst =
  match lst with
  | [] -> []
  | hd :: tl -> (x, hd) :: makepairs x tl


(* Problem 7 Solution *)
(* Author: Edward Campion III *)
let rec binomial n k =
  match (n, k) with
  | (_, 0) -> 1
  | (n, k) when n = k -> 1
  | (n, k) -> binomial (n - 1) (k - 1) + binomial (n - 1) k


(* Problem 9 Solution *)
(* Author: Edward Campion III *)
let rec undup lst =
  match lst with
  | [] -> []
  | x1 :: x2 :: tl when x1 = x2 -> x1 :: undup tl
  | _ -> raise (Failure "bad input")


(* Problem 10 Solution *)
(* Author: Edward Campion III *)
let rec find_min lst =
  match lst with
  | [] -> raise (Failure "Empty list")
  | [x] -> x
  | x :: xs -> min x (find_min xs)