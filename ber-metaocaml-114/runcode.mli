(* Given a closed code expression, compile and run it, returning
   its result or propagating raised exceptions.
*)

open Codelib

(* Type-check the generated code and return the typed tree.
   Offshoring takes it from here.
*)
val typecheck_code : 'a closed_code -> Typedtree.expression

(* Run closed code by bytecode compiling it and then executing *)
val run_bytecode : 'a closed_code -> 'a

(* Other ways of running are equally possible *)

(* The following two synonyms are for backwards compatibility: 
   They are both compositions of close_code and run_bytecode  *)
val run  : 'a code -> 'a
val (!.) : 'a code -> 'a [@@deprecated "use run"]

(* Add a directory to search for .cmo/.cmi files, needed
   for the sake of running the generated code.
   The directory name may be given as +dir to refer to stdlib.
   The specified directory is prepended to the load_path.
*)
val add_search_path : string -> unit
