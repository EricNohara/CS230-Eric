(*
Assign4-2:
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fun () -> ...
//
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

let theNatPairs: (int*int) stream = fun () ->
  let rec generate_pair i j = 
    if i = 0 && j = 0 then StrCons ((i, j), fun () -> generate_pair i (j+1))
    else if j = 0 then StrCons ((i, j), fun () -> generate_pair 0 (i+1))
    else StrCons ((i, j), fun () -> generate_pair (i+1) (j-1))
  in generate_pair 0 0 







  



  

  