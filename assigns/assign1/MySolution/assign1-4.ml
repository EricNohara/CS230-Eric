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

let insert_digit_helper (chr: char) (str: string): string =
  string_tabulate (string_length str+1) (fun i -> if i = 0 then chr else string_get_at str (i-1))

let intrep_add (ds1: string) (ds2:string): string =
  let length1 = string_length ds1 in 
  let length2 = string_length ds2 in 
  let rec loop i carry answer = 
    if i > length1-1 && i > length1-1 && carry = 0 then answer 
    else if i > length1-1 && i > length1-1 then insert_digit_helper (char_of_digit carry) answer 
    else if i > length1-1 then loop (i+1) 0 (insert_digit_helper (char_of_digit(digit_of_char(string_get_at ds2 i) + carry)) answer)
    else if i > length2-1 then loop (i+1) 0 (insert_digit_helper (char_of_digit(digit_of_char(string_get_at ds1 i) + carry)) answer)
    else loop (i+1) ((digit_of_char(string_get_at ds1 i) + digit_of_char(string_get_at ds1 i) + carry)/10) (insert_digit_helper (char_of_digit((digit_of_char(string_get_at ds1 i) + digit_of_char(string_get_at ds2 i) + carry) mod 10)) answer) in
    if loop 0 0 "" = "0" then ""
    else loop 0 0 ""

