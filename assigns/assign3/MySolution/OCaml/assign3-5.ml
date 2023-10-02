(*
//
Assign3-5:
HX-2023-09-28: 30 points (bonus)
//
Here is an implementation of the famous 8-queen puzzle:
https://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x631.html
//
Please give a NON-RECURSIVE implementation that solves the 8-queen puzzle.
//
Hint: Please think of programming in terms of combinators.
//
//
type board_t =
int * int * int * int * int * int * int * int
//
fun
queen8_puzzle_solve(): board_t list =
(*
returns a list of boards consisting of all the solutions to the puzzle.
*)
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml"

type board_t = int * int * int * int * int * int * int * int

let board_get (bd: board_t) (i: int): int =
  let (x0,x1,x2,x3,x4,x5,x6,x7) = bd in
  match i with
  | 0 -> x0
  | 1 -> x1
  | 2 -> x2
  | 3 -> x3
  | 4 -> x4
  | 5 -> x5
  | 6 -> x6
  | 7 -> x7
  | _ -> -1

let board_set (bd: board_t) (i: int) (j: int): board_t =
  let (x0,x1,x2,x3,x4,x5,x6,x7) = bd in
  match i with
  | 0 -> let x0 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | 1 -> let x1 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | 2 -> let x2 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | 3 -> let x3 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | 4 -> let x4 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | 5 -> let x5 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | 6 -> let x6 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | 7 -> let x7 = j in (x0,x1,x2,x3,x4,x5,x6,x7)
  | _ -> bd

let safety_test1 (i0: int) (j0: int) (i: int) (j: int): bool =
  j0 <> j && (abs (i0 - i) <> abs (j0 - j))
  
let safety_test2 (i0: int) (j0: int) (bd: board_t) (i: int): bool =
  let is_safe = int1_foldleft (i + 1) true (fun is_safe row ->
    is_safe && safety_test1 i0 j0 row (board_get bd row)
  ) in
  is_safe
  
(*intfold left, list fold left, list append, intfold left*)
let queen8_puzzle_solve(): board_t list =
  let search (bd: board_t): board_t list =
    let the_solutions = int1_foldleft 8 [] (fun rowAcc i -> 
    list_foldleft rowAcc [] (fun colAcc sol -> 
      list_append colAcc (int1_foldleft 8 [] (fun lst j -> 
        if safety_test2 i (j+1) sol (i-1) then board_set sol i (j-1) :: lst else lst)))) 
    in the_solutions
  in search (0,0,0,0,0,0,0,0)

  