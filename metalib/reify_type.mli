(* Reifying (and printing) the type of a code expression *)
(* This code illustrates type introspection facilities *)

(* Print code values *)
val describe_type_of : Format.formatter -> ('c,'a) code -> unit
