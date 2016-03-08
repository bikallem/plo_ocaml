open Kaputt.Abbreviations
open Lexer
open Parser
    
(** Parser.
    A [Parser] is a LL(1), top-down recursive descent parser for PL/0 language. The EBNF of the 
    language is as follows. 

    [program  =   block "." .

    block   =   ["CONST" ident "=" number { "," ident "=" number} ";"]
    ["VAR" ident {"," ident} ";"]
    {"PROCEDURE" ident ";" block ";"} statement.

    statement   =   [ident ":=" expression | "CALL" ident | "?" ident | "!" expression
    | "BEGIN" statement {";" statement } "END"
    | "IF" condition "THEN" statement
    | "WHILE" condition "DO" statement ].

    condition   =   "ODD" expression | expression ("="|"#"|"<="|"<"|">"|">=") expression .

    expression  =   ["+"|"-"] term {("+"|"-") term}.

    term  =   factor {("*"|"/") factor}.

    factor  =   ident | number | "(" expression ")".]

    https://en.wikipedia.org/wiki/PL/0. *)

let sample1 = "
VAR x, squ;

PROCEDURE square;
BEGIN
   squ:= x * x
END;

BEGIN
   x := 1;
   WHILE x <= 10 DO
   BEGIN
      CALL square;
      ! squ;
      x := x + 1
   END
END."

let lb = Lexing.from_string

let t_next =
  Test.make_assert_test
    ~title:"test next()"
    (fun () -> (Parser.next (Parser.default_pb "x")))
    (fun pb -> Assert.is_true (pb.lookahead = Lexer.Ident "x"))
    (fun _ ->())                 

let t_factor =
  Test.make_assert_test
    ~title:"parse_factor tests"
    (fun () -> Parser.default_pb "x" )
    (fun pb ->
       let pb = Parser.parse_factor (Parser.next pb) in
       Assert.is_true (pb.lookahead = Eof)
    )
    (fun _ -> ())

let t_3 =
  Test.make_assert_test
    ~title:"parse_expression (x+x)"
    (fun () -> Parser.default_pb "(x+x)")
    (fun pb ->
       let pb = Parser.next pb |> Parser.parse_expression in
       Assert.is_true (pb.lookahead = Eof))
    (fun _ -> ())                 
    
let () = Test.run_tests [t_next;t_factor]
