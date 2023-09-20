#use "./../assign0.ml";;
(*helper function used in the anonymous function to reverse the string one character at a time*)
let rec reverseString (i: int) (str: string): char = 
  string_get (str, i)    (*return the character at the corresponding index value of the string*)

(*wrapper function to create a new string of length cs.length and with the correct contents of the reversed string*)
let stringrev(cs: string): string = 
  let length = string_length cs in 
  string_init length (fun (i) -> reverseString(length-i-1)(cs)) (*length-i-1 will give the corresponding index of the correct char needed to be inserted into the string such that the string reverses*)