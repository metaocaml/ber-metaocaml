(* BER MetaOcaml--specific type checking. 
   Should be eventually merged with typecore...
*)

exception TypeCheckingError

val meta_version : string
(** [meta_version] is the version of BER MetaOCaml*)

val trx_structure: Typedtree.structure -> Typedtree.structure
val longidenttostring : Longident.t -> string
val gensymlongident : Longident.t -> Longident.t
val reset_gensymstring_counter : unit -> unit

val mkcsp : Obj.t -> 
  Typedtree.expression option  -> Longident.t -> Parsetree.expression


(* Particularly useful for printing CSP *)
val print_obj : Format.formatter -> Obj.t -> unit
