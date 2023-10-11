(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)

#use "./../../assign4.ml"
#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream = 
  let rec helper (xnode: 'a gtree list): 'a stream = fun () ->
    match xnode with
    | [] -> StrNil
    | ynode :: xrst -> 
      match ynode with
      | GTnil -> helper xrst ()
      | GTcons(el, yrst) -> StrCons(el, helper (list_append yrst xrst))
  in helper [xs]

let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream = 
  let rec helper (xnode: 'a gtree list): 'a stream = fun () ->
    match xnode with
    | [] -> StrNil
    | ynode :: xrst -> 
      match ynode with
      | GTnil -> helper xrst ()
      | GTcons(el, yrst) -> StrCons(el, helper (list_append xrst yrst))
  in helper [xs]