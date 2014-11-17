(* Given a closed code expression, compile and run it, returning
   its result or propagating raised exceptions.
*)

type 'a closed_code = Trx.closed_code_repr

(* Check that the code is closed and return the closed code *)
val close_code : 'a code -> 'a closed_code

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code:
   If there is a scope extrusion error, it is still useful
   to show the code with the extrusion before throwing the scope-extrusion
   exception.
*)
val close_code_delay_check : 'a code -> 'a closed_code * (unit -> unit)

(* Total: a closed code can always be used in slices, etc. *)
val open_code : 'a closed_code -> 'a code

(* Run closed code by bytecode compiling it and then executing *)
val run_bytecode : 'a closed_code -> 'a

(* Other ways of running are equally possible *)

(* The following two synonyms are for backwards compatibility: 
   They are both compositions of close_code and run_bytecode  *)
val run  : 'a code -> 'a
val (!.) : 'a code -> 'a


