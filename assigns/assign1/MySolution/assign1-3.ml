(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_132 (cs: string): bool =
  let length = string_length cs in
  let rec is_132 s:string i:int =
    if i >= length then
      false
    else
      let a = string_get_at s i in
      let rec find_c j =
       if j >= length s then
          false
        else if string_get_at s j < a then
          true
        else
          find_c (j + 1)
      in
      let rec find_b j =
        if j >= length s then
          false
        else if string_get_at s j > a && find_c (j + 1) then
          true
        else
          find_b (j + 1)
      in
      if find_b (i + 1) then
        true
      else
        is_132 s (i + 1)
  in
  if is_132 cs 0 then false else true
;;