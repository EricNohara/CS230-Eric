(*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*
FYI. The concept of word buddies is used in the following game:
https://xanadu-lang.github.io/xats2js/docgen/CodeBook/Doublet/2020-11-29/
https://github.com/xanadu-lang/xats2js/tree/master/docgen/CodeBook/Doublet
*)
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec list_append_element element xs =
  match xs with
  | [] -> [element]  (* If the list is empty, create a new list with the element. *)
  | head :: tail -> head :: (list_append_element element tail)

let make_buddy (pos:int) (str: string): string list =
  let length = string_length str in

  (* Helper function to generate a buddy word for a given letter *)
  let generate_buddy letter =
    string_tabulate length (fun i -> if i = pos then chr (97 + letter) else string_get_at str i) in

  (* Generate buddy words for each letter using list_foldleft *)
  let buddy_list =
    int1_foldleft 26 [] (fun acc letter ->
      let buddy_word = generate_buddy letter in
      (* list_foldleft acc [buddy_word] (fun acc x -> x :: acc) *)
      if buddy_word = str then acc else buddy_word :: acc
    )
  in buddy_list

let list_of_buddies (word: string): string list =
  let length = string_length word in
  let buddies = int1_foldleft length [] (fun acc pos ->
    let buddy_words = make_buddy pos word in
    list_foldleft acc buddy_words (fun acc x -> x :: acc)
  )
  in buddies


