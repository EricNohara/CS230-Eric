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

let rec string_merge (cs1: string) (cs2: string): string =
  match (cs1, cs2) with
  | ("", cs2) -> cs2    (*if cs1 is empty string, then return cs2*)
  | (cs1, "") -> cs1    (*if cs2 is empty string, then return cs1*)
  | (s1, s2) ->         (*else perform function to merge strings*)
    let c1 = string_get_at s1 0 in    (*get the lengths of string 1 and string 2*)
    let c2 = string_get_at s2 0 in
    if char_tolower c1 <= char_tolower c2 then    (*if the char in string 1 is greater than that of string 2, add that char to the string *)
      string_cons c1 (string_merge (string_tabulate (string_length s1 - 1) (fun i -> string_get_at s1 (i + 1))) s2)
    else
      string_cons c2 (string_merge s1 (string_tabulate (string_length s2 - 1) (fun i -> string_get_at s2 (i + 1))))
    ;;





