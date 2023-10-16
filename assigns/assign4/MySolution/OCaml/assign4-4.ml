(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

(* let list_permute (xs: 'a list): 'a list stream = *)
  (* for every element in the list take it out and map it to the sub permutations (backtracking in ocaml) *)

  let rec insert_everywhere x xs =
    match xs with
    | [] -> [[x]]
    | h::t -> (x :: xs) :: (List.map (fun l -> h :: l) (insert_everywhere x t))
  
  let rec permutations xs =
    match xs with
    | [] -> [[]]
    | x::rest ->
      let perms = permutations rest in
      List.flatten (List.map (insert_everywhere x) perms)
  
  let permute nums =
    permutations nums
  
  
  