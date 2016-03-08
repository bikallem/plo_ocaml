(** Parser.
    A [Parser] is a LL(1), top-down recursive descent parser for PL/0 language. 
    https://en.wikipedia.org/wiki/PL/0. *)
open Lexer
open Ast

exception Syntax_error of string 
(** Raised when [!Parser] encounters an unrecongnized token. *)

type parse_buffer = 
  {lookahead : Lexer.token; (** look-ahead token. *)
   lexbuf : Lexing.lexbuf}  (** lexer buffer. *)
(** Represents a parser buffer used during parsing of various productions. *)

let default_pb s = {lookahead = Lexer.Eof; lexbuf = Lexing.from_string s}
(** Create a default [parse_buffer] with the given string [s]. *)

let next pb = {pb with lookahead = Lexer.next_token pb.lexbuf}
(** Retrieves a new parser buffer with the next lookahead token. *)

let expect_error pb t fname =
  let la_str = show_token pb.lookahead and e_str = show_token t in
  let err_msg = Printf.sprintf "Syntax Error. Expected token '%s', however received '%s' in %s.\n" e_str la_str fname in 
  let e = Syntax_error err_msg in
  raise e

(** Throws [Parse_error]. *)
let error pb fname =
  let la_str = show_token pb.lookahead in 
  let err_msg = Printf.sprintf "Syntax Error. Unexpected token '%s'\n" la_str in
  let e = Syntax_error err_msg in
  raise e            

let is_same t1 t2 =
  match t1, t2 with 
  | Ident _, Ident _ -> true
  | Number _, Number _ -> true
  | a, b when a = b -> true
  | _ , _ -> false
(** Returns [true] if two [Lexer.token]s are the same type, [false] otherwise. *)

let is_token_in l t = List.exists (is_same t) l
(** Returns true if token 't' is in [list Lexer.token] 'l'. false otherwise. *)

let expect t pb =
  let expected = is_same t pb.lookahead in
  if expected then (next pb, expected) else (pb, expected)

(** Expects the given token [t] to match the [pb.lookahead] token in [pb]. Raises 'Syntax_error' exception
    if the two tokens donot match. *)

(* factor = ident | number | "(" expression ")". *)
let rec parse_factor pb = 
  match pb.lookahead with
  | Ident id -> (next pb, Identifier id)
  | Number num ->(next pb, Number num)
  | Lparen ->
    let (pb, e) = next pb |> parse_expression in
    let (pb, expected) = expect Rparen pb in
    if expected then (pb, Expr e) else expect_error pb Rparen "parse_factor()"
  | _ -> error pb "parse_factor()"

(* term = factor {("*"|"/") factor}. *)
and parse_term pb =
  let rec loop_factors pb facs =
    if pb.lookahead = Lexer.Divide then
      let (pb, fac) = next pb |> parse_factor in
      loop_factors pb ((Ast.Divide, fac)::facs)
    else if pb.lookahead = Lexer.Times then
      let (pb, fac) = next pb |> parse_factor in
      loop_factors pb ((Ast.Multiply, fac)::facs)
    else (pb, facs) in
  let (pb, fac) = parse_factor pb in
  let (pb, facs) = loop_factors pb [] in
  let term = (fac, facs) in 
  (pb, term)

(* expression = ["+"|"-"] term {("+"|"-") term} . *)
and parse_expression pb =
  let p_start_term pb =    
    if pb.lookahead = Lexer.Plus then
      let (pb, t) = next pb |> parse_term in 
      (pb, (Some Ast.Plus, t))
    else if pb.lookahead = Lexer.Minus then
      let (pb, t) = next pb |> parse_term in
      (pb, (Some Ast.Minus, t))
    else
      let (pb, t) = parse_term pb in
      (pb, (None, t)) in

  let rec loop_terms pb terms =   
    match pb.lookahead with
    | Lexer.Plus ->
      let (pb, t) = next pb |> parse_term in 
      loop_terms pb ((Ast.Plus, t)::terms)
    | Lexer.Minus ->
      let (pb, t) = next pb |> parse_term in
      loop_terms pb ((Ast.Minus, t)::terms)
    | _ -> (pb, terms) in 
  let (pb, start_term) = p_start_term pb in
  let (pb, terms) = loop_terms pb [] in 
  let expr = (start_term, terms) in
  (pb, expr)

(* condition = "ODD" expression | expression ("="|"#"|"<="|"<"|">"|">=") expression . *)
let parse_condition pb =
  let logical_op pb =
    match pb.lookahead with
    | Lexer.Equal -> (next pb, Ast.Equal)
    | Lexer.NotEqual -> (next pb, Ast.NotEqual)
    | Lexer.LessThan -> (next pb, Ast.LessThan)
    | Lexer.LessThanEql -> (next pb, Ast.LessThanEql)
    | Lexer.GreaterThan -> (next pb, Ast.GreaterThan)
    | Lexer.GreaterThanEql -> (next pb, Ast.GreaterThanEql)
    | _ ->
      let err_msg = Printf.sprintf "Syntax Error. Expected 'logical_op' token type (\"=\"|\"#\"|\"<=\"|\"<\"|\">\"|\">=\"). Received '%s' instead." (Lexer.show_token pb.lookahead) in
      raise (Syntax_error err_msg)
  in 
  if pb.lookahead = Lexer.Odd then
    let (pb, e) = next pb |> parse_expression in
    (pb, Ast.Odd e)
  else
    let (pb, left_e) = parse_expression pb in
    let (pb, l_op) = logical_op pb in 
    let (pb, right_e) = parse_expression pb
    in
    (pb, Logical (left_e, l_op, right_e))

(* let parse_condition pb = *)
(*   let is_condition_operator = is_token_in [Equal;NotEqual;LessThanEql;LessThan;GreaterThan;GreaterThanEql] in *)
(*   if pb.lookahead = Odd then next pb |> parse_expression *)
(*   else *)
(*     let pb = parse_expression pb in *)
(*     let pb = if is_condition_operator pb.lookahead then next pb else error() in *)
(*     parse_expression pb *)

(* (\* statement =  *)
(*    [ ident ":=" expression  *)
(*    | "CALL" ident  *)
(*    | "?" ident  *)
(*    | "!" expression *)
(*    | "BEGIN" statement {";" statement } "END" *)
(*    | "IF" condition "THEN" statement *)
(*    | "WHILE" condition "DO" statement ].  *)
(* *\) *)
(* let rec parse_statement pb =  *)
(*   match pb.lookahead with *)
(*   | Ident i -> expect Assignment pb |> parse_expression *)
(*   | Call -> expect (Ident "") pb *)
(*   | Read -> expect (Ident "") pb  *)
(*   | Write -> next pb |> parse_expression *)
(*   | Begin ->      *)
(*     let rec loop_stmt pbl = *)
(*       next pbl       *)
(*       |> function *)
(*       | pbl when pbl.lookahead = Semicolon -> next pbl |> parse_statement |> loop_stmt *)
(*       | _ -> pbl *)
(*     in  *)
(*     next pb *)
(*     |> parse_statement *)
(*     |> loop_stmt *)
(*     |> expect End  *)
(*   | If ->  *)
(*     next pb *)
(*     |> parse_condition *)
(*     |> expect Then *)
(*     |> parse_statement *)
(*   | While ->  *)
(*     next pb *)
(*     |> parse_condition *)
(*     |> expect Do *)
(*     |> parse_statement *)
(*   | _ -> pb               (\* Empty statement. *\) *)

(* (\* block   =    *)
(*    ["CONST" ident "=" number { "," ident "=" number} ";"] *)
(*    ["VAR" ident {"," ident} ";"] *)
(*    {"PROCEDURE" ident ";" block ";"} statement. *\) *)
(* let rec parse_block pb = *)
(*   let pb =  *)
(*     match pb.lookahead with *)
(*     | Const ->  *)
(*       let p_const pb = next pb |> expect (Ident "") |> expect Equal |> expect (Number 0) in *)
(*       let rec loop_const pb = *)
(*         next pb *)
(*         |> function  *)
(*         | pb when pb.lookahead = Comma -> p_const pb |> loop_const *)
(*         | _ -> pb *)
(*       in  *)
(*       p_const pb *)
(*       |> loop_const *)
(*     | Var ->  *)
(*       let p_var pb = next pb |> expect (Ident "") in *)
(*       let rec loop_var pb = *)
(*         next pb *)
(*         |> function *)
(*         | pb when pb.lookahead = Comma -> p_var pb |> loop_var *)
(*         | _ -> pb *)
(*       in  *)
(*       p_var pb *)
(*       |> loop_var *)
(*     | Procedure -> *)
(*       let p_proc pb = expect (Ident "") pb |> expect Semicolon |> parse_block in  *)
(*       let rec loop_proc pb =   *)
(*         next pb *)
(*         |> function  *)
(*         | pb when pb.lookahead = Procedure -> p_proc pb |> loop_proc *)
(*         | _ -> pb  *)
(*       in  *)
(*       p_proc pb *)
(*       |> loop_proc *)
(*     | _ -> error() *)
(*   in  *)
(*   parse_statement pb *)

(* (\* program  =   block "."  *\) *)
(* let program pb =    *)
(*   parse_block pb *)
(*   |> expect Period  *)

(* (\* Main entry point to the PL/O parser. *\) *)
(* let parse_plo lb =  *)
(*   let pb = {lookahead = Lexer.Eof; lexbuf = lb} *)
(*   in  *)
(*   next pb  *)
(*   |> program *)
