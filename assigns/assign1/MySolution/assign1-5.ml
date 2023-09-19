(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let insert_digit_helper (chr: char) (str: string): string =
  string_init (string_length str+1) (fun i -> if i = string_length str then chr else string_get_at str (i))

let rec count_remaining_helper (start: int) (i: int) (str: string) (root: char) (simulated: string): string =
  if i = (string_length str) then simulated
  else if string_get_at str i < root then simulated
  else if i > start && (string_get_at str i < string_get_at str (i-1)) then simulated 
  else count_remaining_helper start (i+1) str root (insert_digit_helper (string_get_at str i) simulated)

let rec count_sim_helper (trav: int) (i: int) (str: string) (root: char) (simulated: string) (current: string): string =
  if i = string_length str then simulated
  else if string_get_at str trav > string_get_at str i then count_sim_helper trav (i+1) str root current current
  else if string_get_at str i < root then count_sim_helper trav (i+1) str root current current
  else if i > (trav+1) && string_get_at str trav <= string_get_at str i && (string_get_at str i < string_get_at str (i-1))  then count_sim_helper trav (i+1) str root current current
  else count_remaining_helper trav (i+1) str root (insert_digit_helper (string_get_at str i) simulated)

let string_longest_ascend (xs: string): string =
  let length = string_length xs in
  let rec loop root trav max current =
    if root = (length-1) then max
    else if trav = length then 
      if string_length current > string_length max then loop (root+1) (root+2) (current) (string_init 1 (fun i -> string_get_at xs (root+1)))
      else loop (root+1) (root+2) max (string_init 1 (fun i -> string_get_at xs (root+1)))
    else if string_get_at xs trav < string_get_at xs root then 
      if string_length current > string_length max then loop root (trav+1) (current) (string_init 1 (fun i -> string_get_at xs root))
      else loop root (trav+1) max (string_init 1 (fun i -> string_get_at xs root))
    else if string_get_at xs trav < string_get_at xs (trav-1) then    
      if string_length current > string_length max then 
        let sim1 = (string_length(count_sim_helper (trav-1) trav xs (string_get_at xs root) "" current)) in
        let sim2 = (string_length (count_remaining_helper trav trav xs (string_get_at xs root) ""))+1 in
        if sim2 <= sim1 then      
        loop root (trav+1) current (string_init 2 (fun i -> if i = 0 then string_get_at xs root else string_get_at xs(trav-1))) 
        else loop root (trav+1) max (string_init 2 (fun i -> if i = 0 then string_get_at xs root else string_get_at xs trav))
      else 
        if (string_length (count_remaining_helper trav trav xs (string_get_at xs root) ""))+1 < string_length current then      
          loop root (trav+1) max (string_init 2 (fun i -> if i = 0 then string_get_at xs root else string_get_at xs (trav-1))) 
        else loop root (trav+1) max (string_init 2 (fun i -> if i = 0 then string_get_at xs root else string_get_at xs trav))
    else 
      loop root (trav+1) max (insert_digit_helper (string_get_at xs trav) current) in
    loop 0 1 "" (string_init 1 (fun i -> string_get_at xs i))
       

  