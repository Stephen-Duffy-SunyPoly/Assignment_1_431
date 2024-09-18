(* Problem 1 Solution *)
(* Author: Stephen Duffy *)
let bigtest lst = 
  match lst with
  | [] -> 1
  | [hd] -> 2
  | hd::tl -> 0

(* Problem 2 Solution *)
(* Author: Stephen Duffy *)
let rotate list = 
  match list with
  | [] -> []
  | [hd] -> [hd]
  | hd::tl -> tl @ [hd]

(* Problem 4 Solution *)
(* Author: Stephen Duffy *)
let rec removeit list y = 
  match list with
  | [] -> []
  | [hd] -> if hd = y then [] else [hd]
  | hd::tl -> if hd = y then removeit(tl) (y) 
    else hd :: removeit(tl) (y)

(* Problem 5 Solution *)
(* Author: Stephen Duffy *)
let rec countbinarry list = 
  match list with 
  | [] -> 0
  | [hd] -> (
    match hd with 
    | (0 | 1) -> 1
    | x -> 0 
    )
  | hd :: tl -> (
    match hd with
    | (0 | 1) -> 1 + countbinarry tl
    | x -> countbinarry tl
    )
(*this was a tricky one but I figured it out eventualy*)

(* Problem 8 Solution *)
(* Author: Stephen Duffy *)
let rec dup list =
  match list with
  | [] -> []
  | [hd] -> [hd;hd]
  | hd::tl -> hd :: hd :: dup tl