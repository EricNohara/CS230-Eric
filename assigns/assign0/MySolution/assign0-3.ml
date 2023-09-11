let rec getDigit(i: int)(i0: int): char =
  if i = 0 then Char.chr (48+(i0 mod 10)) else getDigit(i-1)(i0/10)   (*when i = 0, return the char of the current ones digit, else call the function again with one less digit in i0*)
  
let rec intLength(i0: int): int = 
  if i0 < 10 then 1 else 1 + intLength(i0 / 10)   (*if at the ones digit, return *)
    

let int2str(i0: int): string = 
  let length = intLength(i0) in
  String.init (length) (fun (i) -> getDigit(length-i-1)(i0))    (*initialize a string of the length of i0 and the correct digits in order*)