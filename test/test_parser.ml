open OUnit2
open Lexer

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

let lb = Lexing.from_string sample1


