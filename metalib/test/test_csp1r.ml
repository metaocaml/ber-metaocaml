(* See test_csp1.{ml,mli} *)

let x = Test_csp1.of_string ""
let y = Runcode.run .< Test_csp1.to_string x >.
let () = Printf.printf "Done %s\n" y
(*
   $ metaocamlc abstract.mli abstract.ml test.ml
   $ ./a.out
   File "_none_", line 1:
   Error: This expression has type string but an expression was expected of type
            Test_csp1.t
*)
