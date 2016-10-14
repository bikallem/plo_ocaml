

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

module To_test = struct 
  let capit letter = Astring.Char.Ascii.uppercase letter 
  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end 


let capit() =
  Alcotest.(check char) "Check A" 'A' (To_test.capit 'a')

let plus() =
  Alcotest.(check int) "Sum equals to 7" 7 (To_test.plus [1;1;2;3])

let test_set = [
  "\xF0\x9F\x90\xAB Capitalize", `Quick, capit;
  "Add entries"                , `Slow , plus;
]  

let () = 
  Alcotest.run "My first test" [
    "test_1", test_set;
    "test_2", test_set;
  ]

let ident_test() = ()
  (* Alcotest.check Lexer.token "Check Ident" (Lexer.Ident "x") (Lexer.Ident "x")(* (Parser.default_pb "x" |> Parser.next) *) *)

let l = Lexer.Ident "x"
Lexer.to_string l


(*  

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

let t_3 =
  let pb = Parser.default_pb "(x+x)" in
  let (pb, e1) = 

    let () = Test.run_tests [t_next;t_factor]
 *)