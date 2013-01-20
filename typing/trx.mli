(* BER MetaOCaml compilation
   Transforming the Typedtree to eliminate brackets, escapes and
   run, replacing them with calls to ordinary OCaml functions
   to build the code representation (that is, Parsetree).
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
val sample_loc : Location.t

        (* Run-time quotator *)
val dyn_quote  : Obj.t -> Longident.t Location.loc -> Parsetree.expression

val lift_constant_int  : int  -> Parsetree.expression
val lift_constant_char : char -> Parsetree.expression
val lift_constant_bool : bool -> Parsetree.expression

val build_assert : Location.t -> Parsetree.expression -> Parsetree.expression
val build_lazy   : Location.t -> Parsetree.expression -> Parsetree.expression
val build_bracket : Location.t -> Parsetree.expression -> Parsetree.expression
val build_escape  : Location.t -> Parsetree.expression -> Parsetree.expression
val build_run     : Location.t -> Parsetree.expression -> Parsetree.expression

val build_sequence : 
  Location.t -> Parsetree.expression -> Parsetree.expression -> 
  Parsetree.expression
val build_while : 
  Location.t -> Parsetree.expression -> Parsetree.expression -> 
  Parsetree.expression
val build_when : 
  Location.t -> Parsetree.expression -> Parsetree.expression -> 
  Parsetree.expression

val build_apply : Location.t -> 
                    (Asttypes.label * Parsetree.expression) array -> 
                    Parsetree.expression

val build_tuple : 
  Location.t -> Parsetree.expression array -> Parsetree.expression
val build_array : 
  Location.t -> Parsetree.expression array -> Parsetree.expression
val build_ifthenelse : 
  Location.t -> 
  Parsetree.expression -> Parsetree.expression -> Parsetree.expression option ->
  Parsetree.expression
val build_construct :
 Location.t -> Longident.t Location.loc -> Parsetree.expression array -> bool ->
 Parsetree.expression
val build_record :
 Location.t -> (Longident.t Location.loc * Parsetree.expression) array ->
 Parsetree.expression option -> Parsetree.expression
val build_field :
 Location.t -> Parsetree.expression -> Longident.t Location.loc -> 
 Parsetree.expression
val build_setfield :
 Location.t -> Parsetree.expression -> Longident.t Location.loc -> 
   Parsetree.expression -> Parsetree.expression
val build_variant :
 Location.t -> string -> Parsetree.expression option -> Parsetree.expression
val build_send :
 Location.t -> Parsetree.expression -> string -> Parsetree.expression
val build_open :
 Location.t -> Longident.t Location.loc -> Parsetree.expression -> 
 Parsetree.expression

(*
val longidenttostring : Longident.t -> string
val gensymlongident : Longident.t -> Longident.t
val reset_gensymstring_counter : unit -> unit
*)
