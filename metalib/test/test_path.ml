(* Testing of setting of .cmo path *)
(* The need for setting the path was pointed out by Nicolas Ojeda Bar.
   The following is his simple example.
   We assume that test_path_a.cmo and test_path_a.cmi are moved
   to the tmp/
*)

let _ =
  Runcode.run .< Test_path_a.A >.

Printf.printf "Done\n"
