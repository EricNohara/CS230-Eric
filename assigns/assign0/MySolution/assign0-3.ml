let rec getDigit(i: int): int = 

let rec intLength(i: int): int = 
  if i < 10 then i else 1 + intLength(i / 10)   (*if at the ones digit, return *)
    

let int2str(i0: int): string = 
  String.init(intLength(i0)) (fun i -> getDigit(i))