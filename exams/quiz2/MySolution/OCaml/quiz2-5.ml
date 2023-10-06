(* ************************************************ *)

(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

exception Empty
let list_last(xs: 'a list): 'a = 
  list_foldright xs () (fun acc el -> 
    match (xs, el) with
    | [], ()  -> raise Empty
    | hd :: tl, el -> if hd = el then el else ()
    )
