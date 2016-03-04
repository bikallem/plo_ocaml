(**

   LL(1) top-down recursive descent parser for PL/0 language.


   program 	= 	block "." .

   block 	= 	["CONST" ident "=" number { "," ident "=" number} ";"]
   ["VAR" ident {"," ident} ";"]
   {"PROCEDURE" ident ";" block ";"} statement.

   statement 	= 	[ident ":=" expression | "CALL" ident | "?" ident | "!" expression
   | "BEGIN" statement {";" statement } "END"
   | "IF" condition "THEN" statement
   | "WHILE" condition "DO" statement ].

   condition 	= 	"ODD" expression | expression ("="|"#"|"<="|"<"|">"|">=") expression .

   expression 	= 	["+"|"-"] term {("+"|"-") term}.

   term 	= 	factor {("*"|"/") factor}.

   factor 	= 	ident | number | "(" expression ")".

   http://emotion.inrialpes.fr/people/lehy/ll1.html

*)

open Lexer
exception Parse_error of string 

(* Parser buffer. Holds lookahead token and Lexing.lexbuf. *)
type parse_buffer = 
  {lookahead : Lexer.token; (* look-ahead token. *)
   lexbuf : Lexing.lexbuf}

(* Retrieve a new parser buffer with the next lookahead token. *)
let next pb = {pb with lookahead = Lexer.next_token pb.lexbuf}

let parse file = 
  (* let inp = open_in file and lexbuf = Lexing.from_channel inp in 
     	let tok =  *)
  Printf.printf "\nParsing input '%s' ... \n" file

let error () = raise (Parse_error "\nUnexpected Symbol.")

(* Are the two tokens of same type ? *)
let is_same t1 t2 = 
  match t1, t2 with 
  | Ident _, Ident _ -> true
  | Number _, Number _ -> true
  | a, b when a = b -> true
  | _ , _ -> false

(* Returns true if token 't' is in token list 'l'. false otherwise. *)
let is_token_in l t = List.exists (is_same t) l

(* Expects the given token 't' to match the lookahead token in 'pb'. Raises 'Parse_error' exception
   	if the two tokens donot match. *)
let expect t pb = 
  let pb = next pb in 
  if is_same t pb.lookahead then pb 
  else error()

(* factor = ident | number | "(" expression ")". *)
let rec parse_factor pb = 
  match pb.lookahead with
  | Ident _ | Number _ -> next pb
  | Lparen -> 
    parse_expression pb
    |> expect Rparen
  | _ -> error ()

(* expression = ["+"|"-"] term {("+"|"-") term} . *)
and parse_expression pb =
  let is_plus_minus = is_token_in [Plus;Minus] in
  let rec loop pbl =
    let pbl = next pbl in
    if is_plus_minus pbl.lookahead then loop (parse_term pbl) else pbl 
  in   
  let pb = if is_plus_minus pb.lookahead then next pb else pb
  in  
  parse_term pb
  |> loop	

(* term = factor {("*"|"/") factor}. *)
and parse_term pb =
  let is_times_div = is_token_in [Times;Divide] in 
  let rec loop pbl =
    let pbl = next pbl in 
    if is_times_div pbl.lookahead then loop (parse_factor pbl) else pbl
  in 
  parse_factor pb
  |> loop 

(* condition = "ODD" expression | expression ("="|"#"|"<="|"<"|">"|">=") expression . *)
let parse_condition pb =
  let is_condition_operator = is_token_in [Equal;NotEqual;LessThanEql;LessThan;GreaterThan;GreaterThanEql] in
  if pb.lookahead = Odd then next pb |> parse_expression
  else         
      let pb = parse_expression pb in 
      let pb = if is_condition_operator pb.lookahead then next pb else error() in
      parse_expression pb

(* statement =   
   [ ident ":=" expression 
   | "CALL" ident 
   | "?" ident 
   | "!" expression
   | "BEGIN" statement {";" statement } "END"
   | "IF" condition "THEN" statement
   | "WHILE" condition "DO" statement ]. 
*)
let rec parse_statement pb = 
  match pb.lookahead with
  | Ident i -> expect Assignment pb |> parse_expression
  | Call -> expect (Ident "") pb
  | Read -> expect (Ident "") pb 
  | Write -> next pb |> parse_expression
  | Begin ->     
    let rec loop_stmt pbl =
      next pbl      
      |> function
      | pbl when pbl.lookahead = Semicolon -> next pbl |> parse_statement |> loop_stmt
      | _ -> pbl
    in 
      next pb
      |> parse_statement
      |> loop_stmt
      |> expect End 
  | If -> 
    next pb
    |> parse_condition
    |> expect Then
    |> parse_statement
  | While -> 
    next pb
    |> parse_condition
    |> expect Do
    |> parse_statement
  | _ -> pb               (* Empty statement. *)


