(* Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
Note that you are not allowed to use string concatenation
or your solution is disqualified. *)

let rec reverseString (i: int) (str: string): char = 
  String.get(str)(i)

let stringrev(cs: string): string = 
  let length = String.length (cs) in 
  String.init (length) (fun (i) -> reverseString(length-i-1)(cs))