(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let rec
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

(* HELPER FUNCTIONS *)
let list_map = 
  fun xs -> foreach_to_map_list(list_foreach)(xs)

let prepend_all x lst = 
  list_map lst (fun l -> x :: l)

let rec choose_item n xs =
  match (n, xs) with
  | (0, _) -> [[]]
  | (_, []) -> []
  | (k, y::ys) -> list_append (prepend_all y (choose_item (k - 1) ys)) (choose_item k ys)

(* MAIN WRAPPER FUNCTION *)
let list_nchoose (xs: 'a list) (n0: int): 'a list list =
  if n0 < 0 then [] else choose_item n0 xs




  