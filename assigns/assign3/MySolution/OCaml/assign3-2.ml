(*
Assign3-2:
HX-2023-09-26: 10 points
list_foldleft
//
The function [list_subsets]
returns all the subsets of a given
set (where sets are represented as lists)
//
let rec
list_subsets
(xs: 'a list): 'a list list =
(
match xs with
  [] -> [[]]
| x1 :: xs ->
  let res = list_subsets(xs) in
    res @ list_map(res, fun(xs) -> x1 :: xs)
)
//
Please give a NON-RECURSIVE implementation of
list_subsets based on list-combinators. Note that
the order of the elements in a list representation
of a set is insignificant. For instance, [1,2] and
[2,1] represents the same set {1,2}.
//
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

let list_subsets (xs: 'a list): 'a list list =
  let subset_acc = [[]] in
  let add_element (element: 'a) (subsets: 'a list list) =
    list_foldleft subsets [] (fun acc subset -> (element :: subset) :: acc) in
    list_foldleft xs subset_acc (fun subsets x -> list_append subsets (add_element x subsets))

    