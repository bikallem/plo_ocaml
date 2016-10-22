open Lexer

(* let lexbuf = Lexing.from_string

let test_var _ = 
  let lb = lexbuf "var" in 
  let tok1 = Lexer.next_token lb in  	
  assert_equal tok1 Var	

let test_ident _ = 
  let lb = lexbuf "var vart" in 
  let tok1 = Lexer.next_token lb |> ignore; Lexer.next_token lb in 
  assert_equal tok1 (Ident "vart")

let suite =
  "lexer tests" >:::
  ["test_var">:: test_var;
   "test_ident" >:: test_ident]

let () =
  run_test_tt_main suite
 *)

let t = Lexer.Ident "vaasdd"

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

