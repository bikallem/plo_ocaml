
let _ =
  print_endline "\nPL/0 Parser v1.0 by Bikal Gurung.\n";	
  if (Array.length Sys.argv) < 2 
  then print_endline "usage : \n\tplo <input file>"
  else
    Parser.parse Sys.argv.(1)