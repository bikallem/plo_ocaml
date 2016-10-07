
let _ =
  print_endline "\nPL/0 Parser v1.0 by Bikal Gurung.\n";	
  if (Array.length Sys.argv) < 2 
  then print_endline "usage : \n\tplo <input file>"
  else  	
  let file = Sys.argv.(1) in 
  let input = Pervasives.open_in file in 
  let lb = Lexing.from_channel input in 
  Printf.printf "\nParsing '%s' : " file;
  Parser.parse_plo lb |> ignore
