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

val longidenttostring : Longident.t -> string
val gensymlongident : Longident.t -> Longident.t
val reset_gensymstring_counter : unit -> unit

val mkcsp : Obj.t -> 
  Typedtree.expression option  -> Longident.t -> Parsetree.expression

