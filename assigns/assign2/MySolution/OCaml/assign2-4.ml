(*
//
Assign2-4: 10 points
//
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"
#use "./assign2-1.ml"
#use "./assign2-3.ml"

let list_iforeach (cs: 'a) =
  foldleft_to_iforeach list_foreach cs

let string_sepjoin_list (sep: string) (xs: string list): string =
  string_make_fwork (fun work -> 
    let length = mylist_length xs in
    list_iforeach xs (fun x work -> (string_foreach x (fun work -> work x)); if (i < length-1) then string_foreach sep (fun work -> work sep))
    )