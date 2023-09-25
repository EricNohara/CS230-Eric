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
#use "./../../assign2.ml"

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs work -> 
    let _ = foldleft xs 0 (fun i x -> (work i x; i+1)) in ()

let list_iforeach =
  fun cs -> foldleft_to_iforeach list_foldleft cs

let rec list_length (cs: 'a): int =
  match cs with
  | [] -> 0
  | hd :: tl -> 1 + list_length(tl)

let string_sepjoin_list (sep: string) (xs: string list): string =
  string_make_fwork (fun work -> 
    let length = list_length xs in
    list_iforeach xs (fun i x -> (string_foreach x (fun chr -> work chr); if (i < (length-1)) then string_foreach sep (fun chr -> work chr)))
    )