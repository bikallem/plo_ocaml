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
  let err_msg = Printf.sprintf "Syntax Error. Expected token '%s', however received '%s' in %s().\n" e_str la_str fname in 
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
    if expected then (pb, Expr e) else expect_error pb Rparen "parse_factor"
  | _ -> error pb "parse_factor"

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

(** Returns Ast.identifier if lookahead token in 'pb' in [Lexer.Ident]. Throws 
    Syntax_error otherwise. *)
let identifier fname pb =
  match pb.lookahead with
  | Lexer.Ident id -> (next pb, id)
  | _ -> expect_error pb (Lexer.Ident "x") fname

(** Parse 'statement' productions below: 
    statement =
    [ ident ":=" expression
    | "CALL" ident
    | "?" ident
    | "!" expression
    | "BEGIN" statement {";" statement } "END"
    | "IF" condition "THEN" statement
    | "WHILE" condition "DO" statement ]. *)
let rec parse_statement pb =
  let stmt_identifier = identifier "parse_statement" in 
  match pb.lookahead with
  | Lexer.Ident id -> let (pb, e) = next pb |> parse_expression in (pb, Ast.Assignment (id, e))
  | Lexer.Call -> let (pb, id) = stmt_identifier (next pb) in (pb, Ast.Call id)
  | Lexer.Read -> let (pb, id) = stmt_identifier (next pb) in (pb, Ast.Read id)
  | Lexer.Write -> let (pb, id) = stmt_identifier (next pb) in (pb, Ast.Write id)
  | Lexer.Begin ->
    let rec loop_stmts pb l =
      if pb.lookahead = Semicolon then
        let (pb, stmt) = next pb |> parse_statement in
        loop_stmts pb (stmt::l)
      else
        (pb, l) in
    let (pb, stmt) = parse_statement pb in
    let (pb, stmts) = loop_stmts pb []
    in
    if pb.lookahead = Lexer.End then
      (next pb, Ast.BeginEnd (stmt, stmts))
    else
      expect_error pb (Lexer.End) "parse_statement"
  | Lexer.If ->
    let (pb, cond) = next pb |> parse_condition in
    let (pb, stmt) =
      if pb.lookahead = Lexer.Then then next pb |> parse_statement
      else expect_error pb Lexer.Then "parse_statement"
    in
    (pb, Ast.IfThen (cond,stmt))
  | Lexer.While ->
    let (pb, cond) = next pb |> parse_condition in
    let (pb, stmt) =
      if pb.lookahead = Lexer.Do then next pb |> parse_statement
      else expect_error pb Lexer.Do "parse_statement"
    in
    (pb, Ast.WhileDo (cond, stmt))
  | _ -> (pb, Ast.Empty)

(** Returns an int if the lookahead token is Lexer.Number. Throws Syntax_error otherwise. *)
let number fname pb = 
  match pb.lookahead with
  | Lexer.Number i -> (next pb, i)
  | _ -> expect_error pb (Lexer.Number 0) fname

(* block =
   ["CONST" ident "=" number { "," ident "=" number} ";"]
   ["VAR" ident {"," ident} ";"]
   {"PROCEDURE" ident ";" block ";"} 
   statement. *)
let rec parse_block pb =
  let block_identifier = identifier "parse_block" in
  let parse_constants pb = 
    let p_const pb = 
      let (pb, id) = block_identifier pb in
      let (pb, num) =
        if pb.lookahead = Lexer.Equal then next pb |> number "parse_constants" 
        else expect_error pb Lexer.Equal "parse_constants"
      in
      (pb, (id, num)) in
    let rec loop_constants pb l =
      if pb.lookahead = Lexer.Comma then
        let (pb, const) = next pb |> p_const in
        loop_constants pb (const::l)
      else (pb, l)
    in
    if pb.lookahead = Lexer.Const then 
      let (pb, c) = next pb |> p_const in
      let (pb, cl) = loop_constants pb [] in
      (pb, (c::List.rev cl))
    else
      (pb, []) in  
  let parse_vars pb =
    let rec loop_vars pb l =
      if pb.lookahead = Lexer.Comma then
        let (pb, var) = next pb |> block_identifier in
        loop_vars pb (var::l)
      else (pb, l)
    in
    if pb.lookahead = Lexer.Var then
      let (pb, var) = next pb |> block_identifier in
      let (pb, vars) = loop_vars pb [] in
      (pb, var::(List.rev vars))
    else
      (pb, []) in
  let parse_procedures pb =
    let rec loop_procs pb l =
      if pb.lookahead = Lexer.Procedure then
        let (pb, id) = next pb |> block_identifier in
        let (pb, block) =
          if pb.lookahead = Lexer.Semicolon then next pb |> parse_block
          else expect_error pb Lexer.Semicolon "parse_procedures" in
        let pb =
          if pb.lookahead = Lexer.Semicolon then next pb
          else expect_error pb Lexer.Semicolon "parse_procedures" in
        let proc = Procedure (id, block) 
        in        
        loop_procs pb (proc::l)
      else
        (pb, l)
    in
    loop_procs pb [] in 
  let (pb, constants) = parse_constants pb in
  let (pb, vars) = parse_vars pb in
  let (pb, procs) = parse_procedures pb in 
  let (pb, stmt) = parse_statement pb in
  (pb, Block(constants, vars, procs, stmt))

(* program  =   block "."  *)
let program pb =
  let (pb, block) = parse_block pb in
  if pb.lookahead = Lexer.Period then Program block
  else expect_error pb Lexer.Period "program" 

(* Main entry point to the PL/O parser. *)
let parse_plo lb =
  let pb = {lookahead = Lexer.Eof; lexbuf = lb}
  in
  next pb
  |> program
