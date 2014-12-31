(* Testing of setting of .cmo path *)
(* The need for setting the path was pointed out by Nicolas Ojeda Bar.
   The following is his simple example.
   We assume that test_path_a.cmo and test_path_a.cmi are moved
   to the tmp/

   First of all, to even compile this file we need to pass the flag
   -I /tmp to ocamlc or metaocamlc. Typechecker needs to know where to
   find test_path.cmi. This is needed for compilation, before any code
   is run.
*)


(* Now, if the code is type-checked and the executable is made, to run
   it we still have to specify where to find test_path.cmi amd .cmo files.
   Otherwise, we get the following *run-time* error.

Error: Unbound module Test_path_a

Fatal error: exception Failure("Error type-checking generated code: scope extrusion?")
*)

(*
let _ =
   .< Test_path_a.A >.
*)

(* This is needed to run the generated code *)
let () = Runcode.add_search_path "/tmp"

let _ =
  Runcode.run .< Test_path_a.A >.

let _ = Printf.printf "All Done\n"
