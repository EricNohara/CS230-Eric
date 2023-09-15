(*recursive function that finds the factorial of a given int*)
let rec fact(x: int): int =
  if x > 0 then x * fact(x-1) else 1
  
(*recursive function that finds when x overflows when plugged into the factorial function*)
let rec findoverflow(x: int): int =
  if fact(x) = 0 then x else  findoverflow(x + 1)

let myans = findoverflow(0);;
