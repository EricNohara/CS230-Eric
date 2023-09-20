(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

let sort5: int*int*int*int*int -> int*int*int*int*int =
  let helper a b c d e v =
    if v > a then let v = a in
    if v > b then let v = b in
    if v > c then let v = c in
    if v > d then let v = d in
    if v > e then let v = e in
    return v in 
  fun (a,b,c,d,e) -> (helper a b c d e a, helper a b c d e b, helper a b c d e c, helper a b c d e d, helper a b c d e e)


(* ************************************************ *)
