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

let rec list_append (element: 'a) (lst: 'a list): 'a list =
  match lst with 
  | [] -> [element]
  | hd :: tl -> hd :: (list_append element tl)

let rec list_set_at (xs: 'a list list) (i0: int) (value: 'a list): 'a list list =
  match xs with
  | [] -> []
  | hd :: tl when i0 = 0 -> value :: tl
  | hd :: tl -> hd :: (list_set_at tl (i0 - 1) value)
  
let rec matrix_transpose (xss: 'a list list): 'a list list =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ ->
    let num_cols = list_length (find_hd xss) in
    let transposed_matrix = empty_list_init num_cols in

    let ans = list_foldleft xss (0, transposed_matrix) (
      fun (row, acc) xs -> 
      let updated_row = list_foldleft xs (0, acc) (
        fun (col, acc2) x -> 
        let updated_row = list_get_at acc2 col |> list_append x in
        (col + 1, list_set_at acc2 col updated_row)
      ) |> snd in (row + 1, updated_row)) |> snd in ans

  
  
  
  
  
  



    