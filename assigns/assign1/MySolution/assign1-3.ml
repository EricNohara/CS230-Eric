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
  let rec loop a c b = 
    if length < 3 then true 
    else if (string_get_at cs a < string_get_at cs b && string_get_at cs b < string_get_at cs c) && (a < c && c < b) then false
    else if a = length-3 then true
    else if c = length-2 then loop (a+1) (a+2) (a+3)
    else if b = length-1 then loop a (c+1) (c+2)
    else loop a c (b+1)
  in loop 0 1 2
;;

      
    
    
    