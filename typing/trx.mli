(* BER MetaOcaml--specific type checking. 
   Should be eventually merged with typecore...
*)

exception TrxError of string

val meta_version : string
(** [meta_version] is the version of BER MetaOCaml*)

(* The function to post-process the typed tree and translate away
   brackets and escapes 
*)
val trx_structure: Typedtree.structure -> Typedtree.structure

(* The following names are used by Trx itself to construct a Parsetree
   or as templates to build the Typedtree.
   Trx may generate code the refers to the functions below.
*)

val sample_lid : Longident.t Location.loc  (* A template for lid expressions *)

        (* Run-time quotator *)
val dyn_quote  : Obj.t -> Longident.t Location.loc -> Parsetree.expression

val lift_constant_int : int   -> Parsetree.expression
val lift_constant_char : char -> Parsetree.expression
val lift_constant_bool : bool -> Parsetree.expression


(*
val longidenttostring : Longident.t -> string
val gensymlongident : Longident.t -> Longident.t
val reset_gensymstring_counter : unit -> unit
*)
