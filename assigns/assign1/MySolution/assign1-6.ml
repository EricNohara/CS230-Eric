(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_1324 (cs: string): bool =
  let length = string_length cs in
  let rec loop a c b d= 
    if length < 4 then true 
    else if (string_get_at cs a < string_get_at cs b && string_get_at cs b < string_get_at cs c && string_get_at cs c < string_get_at cs d) && a < c && c < b && b < d then false
    else if a = length-4 then true
    else if c = length-3 then loop (a+1) (a+2) (a+3) (a+4)
    else if b = length-2 then loop a (c+1) (c+2) (c+3)
    else if d = length-1 then loop a c (b+1) (b+2)
    else loop a c b (d+1)
  in loop 0 1 2 3
;;