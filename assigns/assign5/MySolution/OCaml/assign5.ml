#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)
let is_digit (c: char): bool =
  '0' <= c && c <= '9' 

let parse_num (cl: char list): (int * char list) option =
  let rec aux acc cl =
    match cl with
    | c::cs when is_digit c -> let num = int_of_char c - 48 in aux (acc * 10 + num) cs
    | _ -> Some (acc, cl)
  in
  aux 0 cl

let rec parse_expr (cl: char list): (expr * char list) option =
  match cl with
  | '('::'a'::'d'::'d'::' '::xs -> parse_add xs
  | '('::'m'::'u'::'l'::' '::xs -> parse_mul xs
  | _ ->
    match parse_num cl with
    | Some (i, rest) -> Some (Int i, rest)
    | _ -> None

and parse_add (cl: char list): (expr * char list) option =
  let rec aux acc chars =
    match parse_expr chars with
    | Some (e, rest) -> aux (e :: acc) rest
    | None -> Some (Add (list_reverse acc), chars)
  in aux [] cl
    
and parse_mul (cl: char list): (expr * char list) option =
  let rec aux acc chars =
    match parse_expr chars with
    | Some (e, rest) -> aux (e :: acc) rest
    | None -> Some (Mul (list_reverse acc), chars)
  in aux [] cl

let parse (s : string) : expr option = (* YOUR CODE *)
  let cl = trim (string_listize s) in 
  match parse_expr cl with 
  | Some (exp, []) -> Some exp
  | _ -> None


