(*function that checks if a given int is prime*)
let isPrime(n0: int): bool = 
  if n0 <= 1 then false else    (*gaurd clause: if the inputted number is one or less, it cannot be prime*)
    let rec hasdivisor(x: int): bool =
      if x * x > n0 then true   (*if the divisor is greated than the square root of n0, then it is prime*)
      else if n0 mod x = 0 then false   (*if the current divisor divides n0 evenly, then n0 is not prime*)
      else hasdivisor (x+1)   (*else, increase the current divisor by 1*)
    in hasdivisor 2   (*begin search at 2*)
