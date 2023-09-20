(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123", "222987") = "3337110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let int_same_digits_helper (strsmall: string) (strlarge: string): string =
  let lengthsmall = string_length strsmall in
  let lengthlarge = string_length strlarge in
  let padding = lengthlarge-lengthsmall in
  string_init (lengthlarge) (fun i -> if i < padding then '0' else string_get_at strsmall (i-padding))

let insert_digit_helper (chr: char) (str: string): string =
  string_tabulate (string_length str+1) (fun i -> if i = 0 then chr else string_get_at str (i-1))

let intrep_add (ds1: string) (ds2:string): string =
  let length1 = string_length ds1 in
  let length2 = string_length ds2 in

  let rec loop s1 s2 i carry answer = 
    let length = (string_length s1)-1 in
    if i > length && carry = 0 then answer
    else if i > length then insert_digit_helper (char_of_digit carry) answer
    else loop s1 s2 (i+1) ((digit_of_char(string_get_at s1 (length-i)) + digit_of_char(string_get_at s2 (length-i)) + carry)/10) (insert_digit_helper (char_of_digit((digit_of_char(string_get_at s1 (length-i)) + digit_of_char(string_get_at s2 (length-i)) + carry) mod 10)) answer) in

    if length1 > length2 then 
      if loop ds1 (int_same_digits_helper ds2 ds1) 0 0 "" = "0" then "" 
      else loop ds1 (int_same_digits_helper ds2 ds1) 0 0 "" 
    else if length2 > length1 then 
      if loop (int_same_digits_helper ds1 ds2) ds2 0 0 "" = "0" then "" 
      else loop (int_same_digits_helper ds1 ds2) ds2 0 0 ""  
    else 
      if loop ds1 ds2 0 0 "" = "0" then ""
      else loop ds1 ds2 0 0 "" 


