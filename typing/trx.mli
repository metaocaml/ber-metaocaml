(* BER MetaOCaml compilation
   Transforming the Typedtree to eliminate brackets, escapes and
   run, replacing them with calls to ordinary OCaml functions
   to build the code representation (that is, Parsetree).
*)

val meta_version : string
(** [meta_version] is the version of BER MetaOCaml*)

(* The function to post-process the typed tree and translate away
   brackets and escapes 
*)
val trx_structure: Typedtree.structure -> Typedtree.structure

(* Call it whenever we run or print the code *)
val check_scope_extrusion : Parsetree.expression -> Parsetree.expression

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
val dyn_quote  : Obj.t -> Longident.t Location.loc -> Parsetree.expression

val lift_constant_int  : int  -> Parsetree.expression
val lift_constant_char : char -> Parsetree.expression
val lift_constant_bool : bool -> Parsetree.expression

(* Builders of the Parsetree *)
val build_assert  : Location.t -> Parsetree.expression -> Parsetree.expression
val build_lazy    : Location.t -> Parsetree.expression -> Parsetree.expression
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

val build_ident : Location.t -> string Location.loc -> Parsetree.expression
val with_binding_region : 
    string Location.loc -> (string Location.loc -> Parsetree.expression) -> 
    Parsetree.expression
val build_for : 
  Location.t -> string Location.loc -> 
  Parsetree.expression -> Parsetree.expression -> 
  bool -> Parsetree.expression -> Parsetree.expression

val build_fun_simple : 
  Location.t -> string -> string Location.loc -> Parsetree.expression -> 
  Parsetree.expression
val build_fun : 
  Location.t -> string -> string Location.loc array -> 
  Parsetree.pattern list -> Parsetree.expression array ->
  Parsetree.expression

val build_match : 
  Location.t -> Parsetree.expression -> string Location.loc array -> 
  Parsetree.pattern list -> Parsetree.expression array ->
  Parsetree.expression
val build_try : 
  Location.t -> Parsetree.expression -> string Location.loc array -> 
  Parsetree.pattern list -> Parsetree.expression array ->
  Parsetree.expression

val build_let_simple : 
  Location.t -> Asttypes.rec_flag -> string Location.loc -> 
  Parsetree.expression -> Parsetree.expression -> Parsetree.expression
val build_let : 
  Location.t -> Asttypes.rec_flag -> string Location.loc array -> 
  Parsetree.pattern list -> 
  Parsetree.expression array ->         (* the first is the body of let *)
  Parsetree.expression
