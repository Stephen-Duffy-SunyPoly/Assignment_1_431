(*Problem 3 Solution*)
(*Author: Luka Pena*)
let rec remove_last lst =
  match lst with
  | [] -> []
  | [x] -> []
  | x::xs -> x :: (remove_last xs)

(*Problem 11 Solution*)
(*Author: Luka Pena*)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let tree =
  Node (3.14,
        Node (2.718,
              Empty,
              Node (1.414, Empty, Empty)),
        Node (1.618,
              Node (1.732, Empty, Empty),
              Empty));;

(*Problem 12 Solution*)
(*Author: Luka Pena*)
let rec depth tree =
  match tree with
  | Empty -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right);;

(*Problem 13 Solution*)
(*Author: Luka Pena*)
let rec count_leaves tree =
  match tree with
  | Empty -> 0
  | Node (_, left, right) -> count_leaves left + count_leaves right
  | Node (_, Empty, Empty) -> 1;;
