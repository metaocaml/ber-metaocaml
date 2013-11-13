(* BER MetaOCaml compilation
   Transforming the Typedtree to eliminate brackets and escapes,
   replacing them with calls to ordinary OCaml functions
   to build the code representation (that is, Parsetree).
*)

val meta_version : string
(** [meta_version] is the version of BER MetaOCaml*)

(* The function to post-process the typed tree and translate away
   brackets and escapes 
*)
val trx_structure: Typedtree.structure -> Typedtree.structure

(* The following functions operate on untyped code_repr.
   We cannot use the type constructor 'code' here since
   it is not available in the bootstrap compiler.
*)

(* The representation of possibly code: abstract *)
type code_repr

type closed_code_repr = private Parsetree.expression

(* Check that the code is closed and return the closed code *)
val close_code_repr : code_repr -> closed_code_repr

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code:
   If there is a scope extrusion error, it is still useful
   to show the code with the extrusion before throwing the scope-extrusion
   exception.
*)
val close_code_delay_check : code_repr -> closed_code_repr * (unit -> unit)

(* Total: a closed code can always be used in slices, etc. *)
val open_code : closed_code_repr -> code_repr


(* The following names are used by Trx itself to construct a Parsetree
   or as templates to build the Typedtree.
   Trx may generate code the refers to the functions below.
   Therefore, do NOT rename the functions or change their types!
*)

val sample_lid  : Longident.t Location.loc  (* A template for lid expressions *)
val sample_loc  : Location.t
val sample_name : string Location.loc
val sample_pat_list : Parsetree.pattern list
val sample_rec_flag : Asttypes.rec_flag

        (* Run-time quotator *)
val dyn_quote  : Obj.t -> Longident.t Location.loc -> code_repr

val lift_constant_int  : int  -> code_repr
val lift_constant_char : char -> code_repr
val lift_constant_bool : bool -> code_repr

(* Builders of the Parsetree *)
val build_assert   : Location.t -> code_repr -> code_repr
val build_lazy     : Location.t -> code_repr -> code_repr
val build_bracket  : Location.t -> code_repr -> code_repr
val build_escape   : Location.t -> code_repr -> code_repr

val build_sequence : Location.t -> code_repr -> code_repr -> code_repr
val build_while    : Location.t -> code_repr -> code_repr -> code_repr
val build_when     : Location.t -> code_repr -> code_repr -> code_repr

val build_apply : Location.t -> (Asttypes.label * code_repr) array -> code_repr

(* YYY
val build_tuple : 
  Location.t -> code_repr array -> code_repr
val build_array : 
  Location.t -> code_repr array -> code_repr
val build_ifthenelse : 
  Location.t -> 
  code_repr -> code_repr -> code_repr option ->
  code_repr
val build_construct :
 Location.t -> Longident.t Location.loc -> code_repr array -> bool ->
 code_repr
val build_record :
 Location.t -> (Longident.t Location.loc * code_repr) array ->
 code_repr option -> code_repr
val build_field :
 Location.t -> code_repr -> Longident.t Location.loc -> 
 code_repr
val build_setfield :
 Location.t -> code_repr -> Longident.t Location.loc -> 
   code_repr -> code_repr
val build_variant :
 Location.t -> string -> code_repr option -> code_repr
val build_send :
 Location.t -> code_repr -> string -> code_repr
val build_open :
 Location.t -> Longident.t Location.loc -> code_repr -> 
 code_repr
*)
val build_ident : Location.t -> string Location.loc -> code_repr
(*
val with_binding_region : 
    string Location.loc -> (string Location.loc -> code_repr) -> 
    code_repr
val build_for : 
  Location.t -> string Location.loc -> 
  code_repr -> code_repr -> 
  bool -> code_repr -> code_repr

val build_fun_simple : 
  Location.t -> string -> string Location.loc -> code_repr -> 
  code_repr
val build_fun : 
  Location.t -> string -> string Location.loc array -> 
  Parsetree.pattern list -> code_repr array ->
  code_repr

val build_match : 
  Location.t -> code_repr -> string Location.loc array -> 
  Parsetree.pattern list -> code_repr array ->
  code_repr
val build_try : 
  Location.t -> code_repr -> string Location.loc array -> 
  Parsetree.pattern list -> code_repr array ->
  code_repr

val build_let_simple : 
  Location.t -> Asttypes.rec_flag -> string Location.loc -> 
  code_repr -> code_repr -> code_repr
val build_let : 
  Location.t -> Asttypes.rec_flag -> string Location.loc array -> 
  Parsetree.pattern list -> 
  code_repr array ->         (* the first is the body of let *)
  code_repr

*)
