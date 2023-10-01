(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit


let list_map = 
  fun xs -> foreach_to_map_list(list_foreach)(xs)

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs work -> 
    let _ = foldleft xs 0 (fun i x -> (work i x; i+1)) in ()
  
let list_iforeach =
  fun cs -> foldleft_to_iforeach list_foldleft cs  

let rec list_get_at xs i0 = 
    match xs with
    | [] -> []
    | x1 :: xs -> if i0 = 0 then x1 else list_get_at xs (i0-1) 

let rec list_length (xs: 'a list): int =
  match xs with
  | [] -> 0
  | hd :: tl -> 1 + list_length(tl)

let find_hd (xss: 'a list list): 'a list =
  match xss with
  | [] -> []
  | hd :: tl -> hd

let rec empty_list_init (length: int): 'a list list =
  if length = 0 then [] else [] :: empty_list_init (length-1)

let rec matrix_transpose (xss: 'a list list): 'a list list =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ ->
    let num_rows = list_length xss in
    let num_cols = list_length (find_hd xss) in
    let transposed_matrix = empty_list_init num_cols in

    let ans = list_iforeach xss (fun row xs -> 
      (list_iforeach xs (fun col x -> (x :: (list_get_at transposed_matrix col))))) in ans

(* let rec matrix_transpose (xss: 'a list list): 'a list list =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | hd :: tl ->
    let hd_column = list_foldleft xss [] (fun xs -> list_get_at xs 0) in hd_column :: matrix_transpose tl *)


    