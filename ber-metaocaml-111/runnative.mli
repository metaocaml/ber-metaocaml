(* Given a closed code expression, compile it with the *native*
   compiler, linked it in and run returning
   its result or propagating raised exceptions.
*)

open Codelib

(* Run closed code by bytecode compiling it and then executing *)
val run_native : 'a closed_code -> 'a

(* Other ways of running are equally possible *)

(* The following are synonyms are for backwards compatibility: 
   They are both compositions of close_code and run_native  *)
val run  : 'a code -> 'a
val (!.) : 'a code -> 'a [@@deprecated "use run or run_native"]

(* Add a directory to search for .cmo/.cmi files, needed
   for the sake of running the generated code.
   The directory name may be given as +dir to refer to stdlib.
   The specified directory is prepended to the load_path.
*)
val add_search_path : string -> unit


(* Used internally. It has to be exported to communicate with the
   dynamically loaded unit
*)
val result__ : Obj.t option ref
