#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type const = Int of int | Bool of bool | Unit 

type com = Push of const | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt

type coms = com list

(* ---------------------------------------------------------------------------------------------- *)

let parse_int: int parser =
   (let* _ = char '-' in let* x = natural in pure (-x)) <|>
   (let* x = natural in pure x)

let parse_bool: bool parser =
   (keyword "True" >> pure true) <|>
   (keyword "False" >> pure false)

let parse_unit: unit parser =
   (keyword "Unit" >> pure ())

let parse_const =
   (parse_int >>= fun x -> pure(Int x)) <|> (parse_bool >>= fun x -> pure (Bool x)) <|> (parse_unit >> pure Unit)

let parse_com =
   (keyword "Push" >> parse_const >>= fun x -> pure (Push x)) <|>
   (keyword "Pop" >> pure Pop) <|>
   (keyword "Trace" >> pure Trace) <|>
   (keyword "Add" >> pure Add) <|>
   (keyword "Sub" >> pure Sub) <|>
   (keyword "Mul" >> pure Mul) <|>
   (keyword "Div" >> pure Div) <|>
   (keyword "And" >> pure And) <|>
   (keyword "Or" >> pure Or) <|>
   (keyword "Not" >> pure Not) <|>
   (keyword "Lt" >> pure Lt) <|>
   (keyword "Gt" >> pure Gt)

let parse_coms = whitespaces >> many (parse_com << keyword ";")

(* ------------------------------------------------------------------------------------------------- *)
 let int2str(i0: int): string = 
   let rec getDigit(i: int)(i0: int): char =
      if i = 0 then chr (48+(i0 mod 10)) else getDigit(i-1)(i0/10) in
   let rec intLength(i0: int) (numdigits: int): int = 
      if i0 < 10 then numdigits else intLength(i0 / 10) (numdigits+1) in
   if i0 >= 0 then 
     let length = intLength i0 1 in
     string_init (length) (fun (i) -> getDigit(length-i-1)(i0))
   else 
     let i0 = i0 * (-1) in 
     let length = (intLength i0 1)+1 in
     string_init (length) (fun (i) -> if i = 0 then '-' else getDigit(length-i-1)(i0))

let tostring x =
   match x with
   | Int v -> int2str v
   | Bool v -> if v == true then "True" else "False"
   | Unit -> "Unit"

let rec eval_step s t p =
   match p with 
   | Push v::rst -> eval_step (v::s) t rst
   | Pop::rst -> (match s with
                  | [] -> eval_step [] ("Panic"::t) []
                  | hd::tl -> eval_step tl t rst)
   | Trace::rst -> (match s with
                  | [] -> eval_step [] ("Panic"::t) []
                  | hd::tl -> eval_step (Unit::tl) (tostring hd :: t) rst)
   | Add::rst -> (match s with
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | (Int v1)::(Int v2)::tl -> eval_step (Int (v1+v2)::tl) t rst
                  | _ -> eval_step [] ("Panic"::t) [])
   | Sub::rst -> (match s with
                  | (Int v1)::(Int v2)::tl -> eval_step (Int (v1-v2)::tl) t rst
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | Mul::rst -> (match s with
                  | (Int v1)::(Int v2)::tl -> eval_step (Int (v1*v2)::tl) t rst
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | Div::rst -> (match s with
                  | (Int v1)::(Int v2)::tl -> if v2 != 0 then eval_step (Int (v1/v2)::tl) t rst
                                             else eval_step [] ("Panic"::t) []
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | And::rst -> (match s with
                  | (Bool v1)::(Bool v2)::tl -> eval_step (Bool (v1&&v2)::tl) t rst
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | Or::rst -> (match s with
                  | (Bool v1)::(Bool v2)::tl -> eval_step (Bool (v1||v2)::tl) t rst
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | Not::rst -> (match s with
                  | Bool v::tl -> eval_step (Bool (not v)::tl) t rst
                  (* | [] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | Lt::rst -> (match s with
                  | (Int v1)::(Int v2)::tl -> eval_step (Bool (v1<v2)::tl) t rst
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | Gt::rst -> (match s with
                  | (Int v1)::(Int v2)::tl -> eval_step (Bool (v1>v2)::tl) t rst
                  (* | [] -> eval_step [] ("Panic"::t) []
                  | v1::[] -> eval_step [] ("Panic"::t) [] *)
                  | _ -> eval_step [] ("Panic"::t) [])
   | [] -> t

let interp (s : string) : string list option = (* YOUR CODE *)
   let com_list = string_parse parse_coms s in 
      match com_list with
      | Some (program, []) -> Some (eval_step [] [] program)
      | _ -> None