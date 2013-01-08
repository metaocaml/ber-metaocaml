(* Given a closed code expression, compile and run it, returning
   its result or propagating raised exceptions.
*)

type 'a cde = {cde : 'c. ('c,'a) code}  (* Type of the closed code *)

val cde_repr : 'a cde -> Parsetree.expression

val run : 'a cde -> 'a

(* legacy, to be removed ZZZ *)
val run' : Parsetree.expression -> Obj.t

