(* Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
for natural numbers x.
(You can assume that the given string is a sequence of digits)
(And the empty sequence represents the integer 0) *)
let rec accumulate (i: int) (str: string): int =
  if i < 0 then 0 else 
    let digitChar = String.get(str)(i) in let digitValue = Char.code(digitChar) - (Char.code ('0')) in digitValue + 10 * accumulate(i-1)(str)

let str2int(cs: string): int = 
  let length = String.length(cs) in 
    accumulate(length-1)(cs)