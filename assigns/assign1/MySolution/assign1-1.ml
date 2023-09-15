(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
 *)

 let intrev10(n: int): int = 
  let rec intrevhelper(n: int) (acc: int): int = 
    if n = 0 then acc else 
      let digit = n mod 10 in
      let nacc = acc * 10 + digit in
      let n = n / 10 in
      intrevhelper n nacc in 
    intrevhelper n 0


