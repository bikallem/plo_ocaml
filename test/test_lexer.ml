open Alcotest

(* make Lexer.token type testable. *)
let tok_testable = 
  let module M = struct 
    type t = Lexer.token
    let pp fmt t = Format.fprintf fmt "%s" (Lexer.to_string t)
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

(*+----------------- Helpers ---------------------+*)

let tokens s = 
  let lb = Lexing.from_string s in
  let rec loop l = 
    match Lexer.next_token lb with
    | Lexer.Eof -> l
    | t -> loop (t::l)
  in 
  loop []
  |> List.rev 

let tl = Alcotest.list tok_testable
(*+----------------- Helpers ---------------------+*)

(*+----------------- Tests -----------------------+*)

let test_var_tokens () =     
    let inp = "VAR x,y,z;" in
    let actual_toks = tokens inp in 
    let expected_toks = [Lexer.Var; Lexer.Ident "x"; Lexer.Comma; Lexer.Ident "y"; Lexer.Comma;Lexer.Ident "z"; Lexer.Semicolon]  in
    let msg = Printf.sprintf "Check VAR tokens: %s" inp in 
    Alcotest.check tl msg expected_toks actual_toks

open Lexer 

let plo_1 = "
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
END.
"
let test_plo_1_tokens () = 
  let expected_toks =   
    [Var; Ident "x"; Comma; Ident "squ"; Semicolon; 
    Procedure; Ident "square"; Semicolon; 
    Begin; 
    Ident "squ"; Assignment; Ident "x"; Times; Ident "x"; 
    End; Semicolon; 
    Begin; 
    Ident "x"; Assignment; Number 1; Semicolon; 
    While;
    Ident "x"; LessThanEql; Number 10; Do; 
    Begin; 
    Call; Ident "square"; Semicolon;
    Write; Ident "squ"; Semicolon; 
    Ident "x"; Assignment; Ident "x"; Plus; Number 1; 
    End; 
    End; Period] and 
  actual_toks = tokens plo_1 
  in 
  Alcotest.check tl "pl/0 tokens" expected_toks actual_toks

(*+----------------- Tests -----------------------+*)

(*+----------------- Main ------------------------+*)
let test_set = [
  "VAR declaration tokens", `Quick, test_var_tokens;
  "PL/0 tokens",            `Quick, test_plo_1_tokens]

let () = 
  Alcotest.run "PL/0 tokens test" ["tokens", test_set]
(*+----------------- Main ------------------------+*)
