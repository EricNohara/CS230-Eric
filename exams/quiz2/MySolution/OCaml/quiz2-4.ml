(* ************************************************ *)

(*
Q2-4: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldleft. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)
#use "./../../../../classlib/OCaml/MyOCaml.ml"

exception Empty
let list_last(xs: 'a list): 'a = 
  (* First, reverse the list *)
  let list_reverse(xs: 'a list): 'a list =
    list_foldright xs [] (fun acc x -> acc :: x) in
  let rev = list_reverse xs in
  (* match the element to find when it equals the head, then return the current element *)
  list_foldleft rev (fun acc el -> 
    match (rev, el) with
    |[], () -> raise Empty
    | hd :: tl, el -> if hd = el then el else ()
    )
