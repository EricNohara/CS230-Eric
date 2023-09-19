#use "./../../../classlib/OCaml/MyOCaml.ml"

(*helper function to accumulate the int from the digits of the string*)
let rec accumulate (i: int) (str: string): int =
  if i < 0 then 0 else  (*Base case: when the current index points outside the string, return 0*)
    let digitChar = string_get_at str i in 
    let digitValue = (ord digitChar) - (ord '0') in 
    digitValue + 10 * accumulate(i-1)(str)   (*get the correct char at the correct position in the string and convert it to an int, then add it to the total*)

(*wrapper function that creates a string by calling the helper accumulate function with the correct inputs*)
let str2int(cs: string): int = 
  let length = string_length cs in 
    accumulate(length-1)(cs)