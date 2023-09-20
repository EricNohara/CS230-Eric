(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)
#use "./../../../classlib/OCaml/MyOCaml.ml";;


(*Helper function to give the substring of a string starting at the second char of the string*)
let substring_helper (str: string): string = 
  string_init (string_length str - 1) (fun i -> string_get_at str (i + 1))

let rec string_merge (cs1: string) (cs2: string): string =
  match (cs1, cs2) with
  | ("", cs2) -> cs2    (*if cs1 is empty string, then return cs2*)
  | (cs1, "") -> cs1    (*if cs2 is empty string, then return cs1*)
  | (cs1, cs2) ->         (*else perform function to merge strings*)
    let char1 = string_get_at cs1 0 in    (*get the lengths of string 1 and string 2*)
    let char2 = string_get_at cs2 0 in
    if char1 <= char2     (*add the correct char to the string containing the merge *)
    then 
      string_cons char1 (string_merge (substring_helper cs1) cs2)   
    else
      string_cons char2 (string_merge cs1 (substring_helper cs2))
    ;;



