(*
//
Assign4-1:
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: real stream = fun() -> ...
//
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

let the_ln2_stream =
  let rec generate_stream (sign: float) (cur_denom: float) (cur_partial: float): float stream = fun () ->
    let new_denom = cur_denom +. 1.0 in
    let new_sign = sign *. -1.0 in
    let new_sum = (sign /. cur_denom) +. cur_partial in
  StrCons(new_sum, generate_stream new_sign new_denom new_sum)
  in generate_stream 1.0 1.0 0.0

  (* TESTING *)
  (* let StrCons(x1, xs) = the_ln2_stream();;
  let StrCons(x2, xs2) = xs();;
  let StrCons(x3, xs3) = xs2();; *)