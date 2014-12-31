(* BER MetaOCaml compilation
   Transforming the Typedtree to eliminate brackets and escapes,
   replacing them with calls to ordinary OCaml functions
   to build the code representation (that is, Parsetree).
*)

val meta_version : string
(** [meta_version] is the version of BER MetaOCaml*)

(* The function to process the body of the bracket at level n.
   This function `lifts' the Typedtree to the code that will evaluate
   to the corresponding Parsetree.
*)
val trx_bracket : int -> Typedtree.expression -> Typedtree.expression

(* The following functions deal with the representation of brackets,
   escapes and CPS in Parsetree and Typedtree.
   Staging annotations and other levels are distinguished by
   attributes.
   The following functions are used by the typecore.ml as well
   when building the Typedtree.
*)

(* The result of what_stage_attr *)
type stage_attr_elim = 
  | Stage0
  | Bracket of Parsetree.attribute * (* bracket attribute *)
               Parsetree.attributes  (* other attributes  *)
  | Escape  of Parsetree.attribute * (* escape attribute *)
               Parsetree.attributes  (* other attributes  *)
  | CSP     of Parsetree.attribute * 
               Longident.t Location.loc * (* CSP attribute and lid *)
               Parsetree.attributes  (* other attributes  *)

(* Determining if an AST node bears a staging attribute *)
val what_stage_attr : Parsetree.attributes -> stage_attr_elim

(* Build a Typedtree node for brackets or escape (the attribute tells
   which is which)
*)
val texp_braesc : 
  Parsetree.attribute -> Typedtree.expression -> Env.t -> Types.type_expr -> 
  Typedtree.expression

(* Build a Typedtree node for a CSP *)
val texp_csp_raw : 
  Parsetree.attribute -> Asttypes.constant -> Env.t -> Types.type_expr -> 
  Typedtree.expression

(* Staging level 
   It is set via an attribute on the value_description in the Typedtree 
*)
type stage = int                        (* staging level *)
val attr_level : stage -> Parsetree.attribute
val get_level  : Parsetree.attributes -> stage

(* If the attribute is present, the expression is non-expansive 
   We use physical equality comparison, to speed things up
*)
val attr_nonexpansive : Parsetree.attribute

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

(* Adjusting the implementation of stackmarks -- needed when delimited
   control is used (other than mere exceptions).
*)
type stackmark = unit -> bool           (* true if valid *)
type stackmark_region_fn = 
    {stackmark_region_fn : 'w. (stackmark -> 'w) -> 'w}
val set_with_stack_mark : stackmark_region_fn -> unit


(* The following names are used by Trx itself to construct a Parsetree
   or as templates to build the Typedtree.
   Trx may generate code the refers to the functions below.
   Therefore, do NOT rename the functions or change their types!
*)

val loc_none : Location.t
val sample_lid  : Longident.t Location.loc  (* A template for lid expressions *)
val sample_name : string Location.loc
val sample_pat_list : Parsetree.pattern list
val sample_pats_names : Parsetree.pattern list * string Location.loc list

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

val build_apply : Location.t -> (Asttypes.label * code_repr) array -> code_repr

val build_tuple : Location.t -> code_repr array -> code_repr
val build_array : Location.t -> code_repr array -> code_repr
val build_ifthenelse : 
  Location.t -> code_repr -> code_repr -> code_repr option -> code_repr
val build_construct  :
  Location.t -> Longident.t Location.loc -> code_repr array -> code_repr
val build_record :
  Location.t -> (Longident.t Location.loc * code_repr) array ->
  code_repr option -> code_repr
val build_field :
  Location.t -> code_repr -> Longident.t Location.loc -> code_repr
val build_setfield :
  Location.t -> code_repr -> Longident.t Location.loc -> code_repr -> code_repr
val build_variant  : Location.t -> string -> code_repr option -> code_repr
val build_send     : Location.t -> code_repr -> string -> code_repr
val build_open :
  Location.t -> Longident.t Location.loc -> Asttypes.override_flag -> 
  code_repr -> code_repr
val build_fun_nonbinding : 
  Location.t -> string -> Parsetree.pattern list -> 
  (code_repr option * code_repr) array -> code_repr
val build_fun_simple : 
  Location.t -> string -> string Location.loc -> 
  (code_repr -> code_repr) -> code_repr
val build_for : 
  Location.t -> string Location.loc -> code_repr -> code_repr -> 
  bool -> (code_repr -> code_repr) -> code_repr
val build_let_simple_nonrec : 
  Location.t -> string Location.loc -> code_repr -> 
    (code_repr -> code_repr) -> code_repr
val build_fun : 
  Location.t -> string -> 
  (Parsetree.pattern list * string Location.loc list) -> 
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr
val build_let : 
  Location.t -> bool -> 
  (Parsetree.pattern list * string Location.loc list) ->
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr
val build_match : 
  Location.t -> (Parsetree.pattern list * string Location.loc list) -> 
  code_repr -> int ->
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr
val build_try : 
  Location.t -> (Parsetree.pattern list * string Location.loc list) -> 
  code_repr ->
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr
