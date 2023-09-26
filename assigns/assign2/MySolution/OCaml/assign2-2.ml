(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)
#use "./../../assign2.ml"

let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons (x1, xs) -> 1 + mylist_length xs
  | MySnoc (xs, x1) -> 1 + mylist_length xs
  | MyReverse xs -> mylist_length xs
  | MyAppend2 (xs1, xs2) -> (mylist_length xs1) + (mylist_length xs2)


let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a = 
  let length = mylist_length xs in
  if i0 > (length-1) || i0 < 0 then mylist_subscript_exn()
  else 
    match xs with
    | MyNil -> mylist_subscript_exn()
    | MyCons (x1, xs) -> if i0 = 0 then x1 else mylist_get_at xs (i0-1) 
    | MySnoc (xs, x1) -> if i0 = (length-1) then x1 else mylist_get_at xs (i0+1)
    | MyReverse xs -> mylist_get_at xs (length-i0-1)
    | MyAppend2 (xs1, xs2) -> 
      let len1 = mylist_length xs1 in
      if i0 >= len1 then mylist_get_at xs2 (i0-len1) else mylist_get_at xs1 i0

    