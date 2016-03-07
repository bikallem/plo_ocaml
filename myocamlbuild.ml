open Ocamlbuild_plugin
open Ocamlbuild_pack

let rec copy_mlt_files path =
  let elements = Pathname.readdir path in
  Array.iter
    (fun p -> 
       if Pathname.is_directory (path / p) then
	 copy_mlt_files (path / p)
       else if Pathname.check_extension p "mlt" then 
	 let src = path / p in 
	 let dst = !Options.build_dir / path / p in 
	 Shell.mkdir_p (!Options.build_dir / path);
	 Pathname.copy src dst 
       else 
	 ())
    elements

let () = 
  dispatch begin function 
  | After_rules -> 
    copy_mlt_files "src";
    flag ["kaputt"; "pp"]
      (S [A "kaputt_pp.byte"; A"on"; A"camlp4o"])
  | _ -> ()
  end
