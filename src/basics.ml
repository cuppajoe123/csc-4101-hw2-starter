let abs x =
  if x >= 0 then x
    else (-x)

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) =
  (snd tup, fst tup)

let rev_triple (tup : 'a * 'b * 'c) =
  (thd3 tup, snd3 tup, fst3 tup)

let is_odd x =
  match x mod 2 with
  | 0 -> false
  | 1 -> true
  | _ -> true

let is_older (date1: int * int * int) (date2: int * int * int) =
  date1 < date2
  

let to_us_format (date1: int * int * int) =
  (snd3 date1, thd3 date1, fst3 date1)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p =
  if p = 0 then
    1
    else
      x * pow x (p - 1)

let rec fac n =
  if n = 0 then
    1
    else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) =
  if idx = 0 then
    List.hd lst
    else
      get_nth ((idx-1), List.tl lst)

let rec length lst =
  if lst = [] then
    0
    else
      1 + length (List.tl lst)

let larger lst1 lst2 =
  let length1 = length lst1 in
  let length2 = length lst2 in
  if length1 = length2 then []
    else if length1 > length2 then lst1
      else lst2
  
let rec sum_list lst =
  if lst = [] then
    0
    else
      List.hd lst + sum_list (List.tl lst)


let sum lst1 lst2 = sum_list lst1 + sum_list lst2

