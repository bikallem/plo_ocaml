(*
   program = block "." .

   block = [ "const" ident "=" number {"," ident "=" number} ";"]
   [ "var" ident {"," ident} ";"]
   { "procedure" ident ";" block ";" } statement .

   statement = [ ident ":=" expression | "call" ident 
   | "?" ident | "!" expression 
   | "begin" statement {";" statement } "end" 
   | "if" condition "then" statement 
   | "while" condition "do" statement ].

   condition = "odd" expression |
   expression ("="|"#"|"<"|"<="|">"|">=") expression .

   expression = [ "+"|"-"] term { ("+"|"-") term}.

   term = factor {("*"|"/") factor}.

   factor = ident | number | "(" expression ")". *)

open Lexer

type expression =
  | Identifier of (plus_minus option * string)
  | Number of (plus_minus option * int)
  | Addition of expression * expression
  | Subtraction of expression * expression
  | Multiplication of expression * expression
  | Division of expression * expression
  | Odd of expression
  | Conditional of comparision * expression
and plus_minus =
  | Plus
  | Minus

(* type factor = *)
(*   | Identifier of string *)
(*   | Number of int *)
(*   | Expr of expression *)
(* and expression = *)
(*   | PlusMinusTerm of plus_minus_term *)
(*   | Terms of (term * term list) *)
(* and plus_minus_term = *)
(*   | Plus of term *)
(*   | Minus of term *)
(* and term = *)
(*   | Factor of factor *)
(* and times_divide_factor = *)
(*   | Times of in *)
