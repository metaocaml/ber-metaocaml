(*
  The goal of this file is to post-process the Typedtree
  after the type checking and before the code generation to
  get rid of bracket, esc and run. The main function is
  trx_structure, which initiates the traversal and
  transforms every found expression with trx_exp. The real
  transformation is done by trx_bracket.

  For example,
     <succ 1> 
  gets transformed to 
     mkApp <succ> <1> 
  and eventually to
     mkApp (mkIdent "succ") (mkConst 1)
  One may say that we `push the brackets inside'.
  We replace bracket with calls to functions that will construct, at run-time,
  a Parsetree, which is the representation of code type.

  Generally, the Parsetree is constructed when the program is run.
  In some cases we can construct the Parsetree at compile time,
  that is, when this trx.ml is run. Constants like <1> is such a case.
  If we see <1>, or, in terms of trees,
      Texp_bracket (Texp_constant (Constant_int 1))
  we can immediately construct the Parsetree:
      Pexp_constant (Constant_int 1)
  After we construct the Parsetree at compile time, we use CSP to
  pass it over to run-time. At run-time, we merely use the compiled constant.
  This mechanism of building Parsetree at compile-time whenever possible
  is one of the large differences from the previous versions of MetaOCaml.

  Bindings.
  Checking for scope extrusion: stack of currently active ids...

Future-stage identifier x was represented in tree as 
Texp_ident (ident,vd)
We transform x if it was written Pexp_ident li, or
in tree terms
Texp_construct ("Pexp_ident", [

constants (and csp, Assertfalse)
This file is based on trx.ml from the original MetaOCaml,
but it is almost completely re-written.

*)

open Parsetree
open Asttypes
open Misc
open Typedtree
open Types
(*
open Ctype
open Parmatch
open Path
open Ident
open Env
*)



(* BER MetaOCaml version string *)
let meta_version  = "N 100"

exception TrxError of string

(* ------------------------------------------------------------------------ *)
(* Path utilities *)

(* We always use path when available, and convert it to Longident
   when needed -- even if the Typedtree already carries the longident.
   The path is preferred because it is fully qualified for
   external identifiers and it is unambiguous.
   If we open a module, its components can be referred to without
   qualification -- the path will be qualified nevertheless.
   When we build a Parsetree representing the generated code,
   we have to use fully qualified identifiers since the open statement
   in the original code won't be represented in the generated
   Parsetree.
*)

(* Check to see if a path refers to an identifier, exception, or
   constructor that is available from an external module. If so, the run-time
   compiler invoked by .! can get the definition for the identifier from
   a .cmi file. The value of an external identifier can be obtained from
   a .cmo file.
*)
let is_external = function
  | Path.Pident _ -> false              (* not qualified *)
  | Path.Papply _ -> false
  | Path.Pdot(Path.Pident id, _,_) -> Ident.persistent id
  | _             -> false


(* Check to make sure a constructor, label, exception, etc.
   have the name that we can put into AST (Parsetree).
   Local names can't be put into the Parsetree since the type env in which
   they are declared is not represented in the Parsetree.
*)
let check_path_quotable msg path =
  if not (is_external path) then
    raise (TrxError (msg ^ 
     " cannot be used within brackets. Put into a separate file."))

(* Test if we should refer to a CSP value by name rather than by
   value
*)
(* Module identifiers for the modules that are expected to be
   present at run-time -- that is, will be available for
   dynamic linking of the run-time generated code.
   Basically we can assume the standard library.
*)

(* 
let pervasive_idents =
  List.map Ident.create_persistent 
  ["Pervasives"; "Array"; "Printf"; "List"; "String"]

let ident_can_be_quoted = function
  | Path.Pdot(Path.Pident id, _,_) ->
      List.exists (Ident.same id) pervasive_idents
  | _ -> false
*)

let ident_can_be_quoted = is_external   (* Perhaps this is a better version *)

(* Convert a path to an identifier. Since the path is assumed to be
   `global', time stamps don't matter and we can use just strings.
*)
let rec path_to_lid : Path.t -> Longident.t = function
  | Path.Pident i       -> Longident.Lident (Ident.name i)
  | Path.Pdot (p,s,_)   -> Longident.Ldot (path_to_lid p, s)
  | Path.Papply (p1,p2) ->
      Longident.Lapply(path_to_lid p1, path_to_lid p2)

let dummy_lid : string -> Longident.t loc = fun name ->
  Location.mknoloc (Longident.Lident name)

(* Exported. Used as a template for constructing lid expressions *)
let sample_lid = dummy_lid "*sample*"

(* Exported. Used as a template for constructing Location.t expressions *)
let sample_loc = Location.none


(* ------------------------------------------------------------------------ *)
(* Building Texp nodes *)
(* Env.initial is used for all look-ups. Unqualified identifiers
   must be found there. For qualified identifiers, Env.lookup
   functions look things up in the persistent structures, loading them
   up as needed.
*)

(* Should we add memoization? *)

(* Building a node for an identifier with a given (qualified) name *)
let texp_ident : string -> expression = fun name ->
  let lid     = Longident.parse name in
  let (p, vd) = try Env.lookup_value lid Env.initial 
                with Not_found -> fatal_error ("Trx.find_value: " ^ name) in
  { exp_desc = Texp_ident (p,mknoloc lid, vd);
    exp_loc  = Location.none; exp_extra = [];
    exp_type = Ctype.instance Env.initial vd.val_type;
    exp_env  = Env.initial }

(* Building an application *)
let texp_apply : Typedtree.expression -> Typedtree.expression list -> 
 Typedtree.expression_desc = fun f args ->
   Texp_apply(f, List.map (fun arg -> ("",Some arg, Required)) args)



(* building a typical Parsetree node: Pexp_assert of expression*)
let build_assert : Location.t -> Parsetree.expression -> Parsetree.expression = 
  fun l e -> {pexp_loc  = l; pexp_desc = Pexp_assert e}

(* When we translate the typed-treee, we have to manually compile
   the above code 
First, to see the AST for the phrase, invoke the top-level with the flag
-dparsetree. Then
   {pexp_loc  = l; pexp_desc = Pexp_assert e}

gives the parsetree:
let build_assert_ast : Location.t -> Parsetree.expression -> Parsetree.expression = 
{pexp_loc = l1;
 pexp_desc = 
  Pexp_record
        ([(Location.mknoloc (Longident.parse "Parsetree.pexp_loc"), 
           Pexp_ident "l");
         (Location.mknoloc (Longident.parse "Parsetree.pexp_desc"),
           {pexp_loc  = Location.none;
            pexp_desc = Pexp_construct 
                          ((Location.mknoloc (Longident.parse 
                                                "Parsetree.Pexp_assert")),
              Some {pexp_loc = Location.none;
                    pexp_desc = Pexp_ident "e"},
              false)})
        ],
        None)}
type_expression

If building the parsetree on our own, beware! For example, labels in
Texp_record must be sorted, in their declared order!
*)


(* ------------------------------------------------------------------------ *)
(* Dealing with CSP *)

exception CannotLift

(* Analyze the type of the expression and figure out if we can lift it.
   Raise CannotLift if cannot (e.g., the type is polymorphic), or it is too
   much to bother.
   TODO: lists, arrays, option types of liftable types are themselves
   liftable. We can lift many more types. For arrays, check their length.
   If the array is short, it should be lifted. For long arrays,
   building a CSP is better (although it make take a bit longer since
   we will have to invoke dyn_quote at run-time).

   TODO: currently we generate calls to run-time functions like 
   lift_constant_int to do the Parsetree generation. In the future
   we should `inline' those functions -- that is, obtain the Typedtree
   for them and use the tree for building Texp_apply.
*)
let lift_as_literal : 
  Typedtree.expression -> Path.t -> Longident.t loc -> 
  Typedtree.expression_desc = fun exp p li ->
  let exp_ty =
        Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
  match Ctype.repr exp_ty with
    | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_int ->
        texp_apply (texp_ident "Trx.lift_constant_int") [exp]
    | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_char ->
        texp_apply (texp_ident "Trx.lift_constant_char") [exp]
    | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_bool ->
        texp_apply (texp_ident "Trx.lift_constant_bool") [exp]
          (* double and string are handled by dyn_quote *)
    | _ -> raise CannotLift

(* TODO: similarly handle Const_nativeint, Const_int32, Const_int64 *)
let lift_constant_int : int -> Parsetree.expression = fun x -> 
  {pexp_loc  = Location.none;
   pexp_desc = Pexp_constant (Const_int x)}

let lift_constant_char : char -> Parsetree.expression = fun x -> 
  {pexp_loc  = Location.none;
   pexp_desc = Pexp_constant (Const_char x)}

let lift_constant_bool : bool -> Parsetree.expression = fun x -> 
  let b = if x then "true" else "false" in 
  {pexp_loc  = Location.none;
   pexp_desc = Pexp_construct (Location.mknoloc (Longident.Lident b), 
                               None, false)}


(* Lift the run-time value v into a Parsetree for the code that, when
   run, will produce v.
   We do not have the type information for v, but we can examine
   its run-time representation, to decide if we lift it is a source
   literal or as a CSP.

  TODO: also check for double_array_tag
   and create a (structured) constant for a double array
*)
let dyn_quote : Obj.t -> Longident.t loc -> Parsetree.expression =
  fun v li ->
   let dflt = Pexp_cspval(v,li) in        (* By default, we build CSP *)
   let desc = 
    match Obj.is_int v with
    | true -> dflt  (* If v looks like an int, it can represent many things: *)
                    (* can't lift *)
    | false when Obj.tag v = Obj.double_tag ->
      Pexp_constant (Const_float (string_of_float (Obj.obj v)))
    | false when Obj.tag v = Obj.string_tag ->
      Pexp_constant (Const_string (Obj.obj v))
    | _   -> dflt
   in 
   {pexp_loc = li.loc; pexp_desc = desc}

       
(* Build the Typedtree that lifts the variable with the given path and type.
   Since this code receives the type of the variable, we use the
   type to generate the lifting code for that particular type.
   For example, we build the code to convert a float
   0.1 to the Parsetree node Pexp_constant(Const_float "0.1")).
   If we cannot or would not type-dependent lifting and we cannot
   refer to the variable by name (e.g., because it is local),
   we generate the call to the dynamic quoter, dyn_quote.
   The latter will receive the actual value to quote and will generate,
   at run-time, a Parsetree constant or CSP, based on that value.
 *)
let trx_csp : 
  Typedtree.expression -> Path.t -> Longident.t loc ->
  Typedtree.expression_desc = fun exp p li ->
  (* First we try lifting as a constant *)
  try lift_as_literal exp p li 
  with CannotLift ->
  (* Then check if we can pass by reference *)
  if ident_can_be_quoted p then
     let ast = 
         {pexp_loc = exp.exp_loc;
          pexp_desc = Pexp_ident (Location.mkloc (path_to_lid p) li.loc)}
      in Texp_cspval (Obj.repr ast, dummy_lid "*id*")
  else
  (* Otherwise, do the lifting at run-time *)
  let lid_exp = texp_ident "Trx.sample_lid" in (* this fills in the type, etc.*)
  let lid_exp = 
      {lid_exp with exp_desc = Texp_cspval (Obj.repr li, dummy_lid "*csp_li*")}
  in
  texp_apply (texp_ident "Trx.dyn_quote") [exp;lid_exp]

(*
(* based on code taken from typing/parmatch.ml *)

let clean_copy ty =
  if ty.level = Btype.generic_level then ty
  else Subst.type_expr Subst.identity ty

let get_type_path ty tenv =
  let ty = Ctype.repr (Ctype.expand_head tenv (clean_copy ty)) in
  match ty.desc with
  | Tconstr (path,_,_) -> path
  | _ -> fatal_error "Parmatch.get_type_path"

let rec get_type_descr ty tenv =
  match (Ctype.repr ty).desc with
  | Tconstr (path,_,_) -> Env.find_type path tenv
  | _ -> fatal_error "Parmatch.get_type_descr"

let rec get_constr tag ty tenv =
  match get_type_descr ty tenv with
  | {type_kind=Type_variant constr_list} ->
      Datarepr.find_constr_by_tag tag constr_list
  | {type_manifest = Some _} ->
      get_constr tag (Ctype.expand_head_once tenv (clean_copy ty)) tenv
  | _ -> fatal_error "Parmatch.get_constr"

let find_label lbl lbls =
  try
    let name,_,_ = List.nth lbls lbl.lbl_pos in
    name
  with Failure "nth" -> "*Unkown label*"

let rec get_record_labels ty tenv =
  match get_type_descr ty tenv with
  | {type_kind = Type_record(lbls, rep)} -> lbls
  | {type_manifest = Some _} ->
      get_record_labels (Ctype.expand_head_once tenv (clean_copy ty)) tenv
  | _ -> fatal_error "Parmatch.get_record_labels"

let get_constr_name tag ty tenv  = match tag with
| Cstr_exception path -> Path.name path
| _ ->
  try
    let name,_ = get_constr tag ty tenv in name
  with
  | Datarepr.Constr_not_found -> "*Unknown constructor*"

let update_lid lid name =
  match lid with
    Longident.Lident _ -> Longident.Lident name
  | Longident.Ldot (p,_) -> Longident.Ldot (p,name)
  | _ -> fatal_error("Trx.update_lid")

(* XXO: get the constructor lid by getting the path of
   the type and updating it with the constructor name.
   For example, Parsetree.Ppat_any is reconstructed from
   type: Parsetree.pattern_desc
   name: Ppat_any                                *)

XXX use:	check_path_quotable p;

let get_constr_lid tag ty tenv = 
 match tag with
 | Cstr_exception path -> path_to_lid path
 | _ -> let name = get_constr_name tag ty tenv
        and type_path = get_type_path ty tenv
        in update_lid (path_to_lid type_path) name

XXX use: 	check_path_quotable p;

let get_record_lids ty tenv =
  let lbls = get_record_labels ty tenv in
  let type_lid = path_to_lid (get_type_path ty tenv) in
  let label_lid (name,_,_) = update_lid type_lid name
  in List.map label_lid lbls


let map_option f o =
  match o with
    None -> None
  | Some x -> Some (f x) 

let rec map_strict f l =
  match l with
    [] -> []
  | (None::xs) -> map_strict f xs
  | ((Some a):: xs) -> (f a)::(map_strict f xs)
                                
let map_pi2 f p =
  match p with
    (x,y) -> (x, f y)

let map_pi1 f p =
  match p with
    (x,y) -> (f x, y)

let add_ifnew x l =
  if List.mem x l then l else x::l

(* Unqualified indetifiers are looked up in the initial
   environment. Qualified identifiers are looked into (external)
   modules, which are loaded by demand, in Env.find *)
let env0 = Env.initial

let find_type name =
  try
    let lid = Longident.parse name in
    let (path, decl) = Env.lookup_type lid env0 in
    newty (Tconstr(path, [], ref Mnil))
  with Not_found ->
    fatal_error ("Trx.find_type: " ^ name)
let find_constr name =
  try
    let lid = Longident.parse name in
    Env.lookup_constructor lid env0
  with Not_found ->
    fatal_error ("Trx.find_constr: " ^ name)
let find_label name =
  try
    let lid = Longident.parse name in
    Env.lookup_label lid env0
  with Not_found ->
    fatal_error ("Trx.find_label: " ^ name)
let find_value name =
  try
    let lid = Longident.parse name in
    Env.lookup_value lid env0
  with Not_found ->
    fatal_error ("Trx.find_value: " ^ name)

(* ZZZ I wonder if the following really necessary. First of all,
parsing of the identifiers, using Longident.parse above, can be
done outside of lazy. Further, we are looking for identifiers in
the same initial env. Should we just refer to the predefined env?
Especially the types like string and list and their cosntructors.
Predefined env has all this information, and it stays the same.

I guess the point of lazy is to memoize repeated searches, and avoid
searches for infrequent things.
Since things like int, bool and string are going to be used all the time,
we should just look them up eagerly.
Further, should we use Predef.path_int, etc?
*)

let type_bool = lazy (find_type "bool")
let type_location = lazy (find_type "Location.t")
let pathval_location_none = lazy (find_value "Location.none")
let type_constant = lazy (find_type "Asttypes.constant")
let constr_const_int = lazy (find_constr "Asttypes.Const_int")
let constr_const_char = lazy (find_constr "Asttypes.Const_char")
let constr_const_string = lazy (find_constr "Asttypes.Const_string")
let constr_const_float = lazy (find_constr "Asttypes.Const_float")
let constr_const_int32 = lazy (find_constr "Asttypes.Const_int32")
let constr_const_int64 = lazy (find_constr "Asttypes.Const_int64")
let constr_const_nativeint = lazy (find_constr "Asttypes.Const_nativeint")
let constr_cons = lazy (find_constr "::")
let constr_nil = lazy (find_constr "[]")
let constr_none = lazy (find_constr "None")
let constr_some = lazy (find_constr "Some")
let constr_false = lazy (find_constr "false")
let constr_true = lazy (find_constr "true")
let constr_nonrecursive = lazy (find_constr "Asttypes.Nonrecursive")
let constr_recursive = lazy (find_constr "Asttypes.Recursive")
let constr_default = lazy (find_constr "Asttypes.Default")
let constr_upto = lazy (find_constr "Asttypes.Upto")
let constr_downto = lazy (find_constr "Asttypes.Downto")
    
let type_longident_t = lazy (find_type "Longident.t")
let type_parsetree_expression = lazy (find_type "Parsetree.expression")
let type_parsetree_pattern = lazy (find_type "Parsetree.pattern")
let type_parsetree_structure_item = lazy (find_type "Parsetree.structure_item")
let type_parsetree_core_type = lazy (find_type "Parsetree.core_type")
let label_pexp_desc = lazy (find_label "Parsetree.pexp_desc")
let label_pexp_loc  = lazy (find_label "Parsetree.pexp_loc")
let label_ppat_desc = lazy (find_label "Parsetree.ppat_desc")
let label_ppat_loc  = lazy (find_label "Parsetree.ppat_loc")
let label_loc_start = lazy (find_label "Location.loc_start")
let label_loc_end   = lazy (find_label "Location.loc_end")
let label_loc_ghost = lazy (find_label "Location.loc_ghost")
let label_pos_fname = lazy (find_label "Lexing.pos_fname")
let label_pos_lnum = lazy (find_label "Lexing.pos_lnum")
let label_pos_bol = lazy (find_label "Lexing.pos_bol")
let label_pos_cnum = lazy (find_label "Lexing.pos_cnum")
let type_parsetree_expression_desc = lazy (find_type "Parsetree.expression_desc")
let type_parsetree_pattern_desc = lazy (find_type "Parsetree.pattern_desc")
let type_parsetree_structure_item_desc = lazy (find_type "Parsetree.structure_item_desc")
let type_core_type_desc = lazy (find_type "Parsetree.core_type_desc")
let type_list       = lazy (find_type "int")  (* XXO bad hack.  Walid. *)
let type_exp_option = lazy (find_type "int")  (* XXO bad hack.  Walid. *)
let type_rec_flag   = lazy (find_type "Asttypes.rec_flag")
let type_label      = lazy (find_type "string")

let constr_pexp_constant      = lazy (find_constr "Parsetree.Pexp_constant")
let constr_pexp_ident         = lazy (find_constr "Parsetree.Pexp_ident")
let constr_pexp_apply         = lazy (find_constr "Parsetree.Pexp_apply")
let constr_pexp_function      = lazy (find_constr "Parsetree.Pexp_function")
let constr_pexp_match         = lazy (find_constr "Parsetree.Pexp_match")
let constr_pexp_try           = lazy (find_constr "Parsetree.Pexp_try")
let constr_pexp_ifthenelse    = lazy (find_constr "Parsetree.Pexp_ifthenelse")
let constr_pexp_record        = lazy (find_constr "Parsetree.Pexp_record")
let constr_pexp_field         = lazy (find_constr "Parsetree.Pexp_field")
let constr_pexp_setfield      = lazy (find_constr "Parsetree.Pexp_setfield")
let constr_pexp_array         = lazy (find_constr "Parsetree.Pexp_array")
let constr_pexp_sequence      = lazy (find_constr "Parsetree.Pexp_sequence")
let constr_pexp_while         = lazy (find_constr "Parsetree.Pexp_while")
let constr_pexp_for           = lazy (find_constr "Parsetree.Pexp_for")
let constr_pexp_when          = lazy (find_constr "Parsetree.Pexp_when")
let constr_pexp_send          = lazy (find_constr "Parsetree.Pexp_send")
let constr_pexp_new           = lazy (find_constr "Parsetree.Pexp_new")
let constr_pexp_let           = lazy (find_constr "Parsetree.Pexp_let")
let constr_pexp_bracket       = lazy (find_constr "Parsetree.Pexp_bracket")
let constr_pexp_escape        = lazy (find_constr "Parsetree.Pexp_escape")
let constr_pexp_run           = lazy (find_constr "Parsetree.Pexp_run")
let constr_pexp_assert        = lazy (find_constr "Parsetree.Pexp_assert")
let constr_pexp_assertfalse   = lazy (find_constr "Parsetree.Pexp_assertfalse")
let constr_pexp_lazy      = lazy (find_constr "Parsetree.Pexp_lazy")
let constr_pexp_tuple         = lazy (find_constr "Parsetree.Pexp_tuple")
let constr_pexp_variant       = lazy (find_constr "Parsetree.Pexp_variant")
let constr_pexp_construct     = lazy (find_constr "Parsetree.Pexp_construct")
let constr_pexp_cspval        = lazy (find_constr "Parsetree.Pexp_cspval")
let constr_pexp_letmodule     = lazy (find_constr "Parsetree.Pexp_letmodule")
let constr_ppat_construct     = lazy (find_constr "Parsetree.Ppat_construct")
let constr_ppat_record        = lazy (find_constr "Parsetree.Ppat_record")
let constr_ppat_or            = lazy (find_constr "Parsetree.Ppat_or")
let constr_ppat_lazy          = lazy (find_constr "Parsetree.Ppat_lazy")
let constr_ppat_array         = lazy (find_constr "Parsetree.Ppat_array")
let constr_ppat_var           = lazy (find_constr "Parsetree.Ppat_var")
let constr_ppat_any           = lazy (find_constr "Parsetree.Ppat_any")
let constr_ppat_constant      = lazy (find_constr "Parsetree.Ppat_constant")
let constr_ppat_alias         = lazy (find_constr "Parsetree.Ppat_alias")
let constr_ppat_variant       = lazy (find_constr "Parsetree.Ppat_variant")
let constr_ppat_tuple         = lazy (find_constr "Parsetree.Ppat_tuple")
let constr_longident_lident   = lazy (find_constr "Longident.Lident")
let constr_longident_ldot     = lazy (find_constr "Longident.Ldot")
let constr_longident_lapply   = lazy (find_constr "Longident.Lapply")

let pathval_trx_longidenttostring = lazy (find_value "Trx.longidenttostring")
let pathval_trx_gensymlongident = lazy (find_value "Trx.gensymlongident")

let trx_longidenttostring exp =
  let (p, v) = Lazy.force pathval_trx_longidenttostring in
  { exp with exp_type = instance v.val_type;
    exp_desc = Texp_ident(p, v) }

let trx_gensymlongident exp =
  let (p, v) = Lazy.force pathval_trx_gensymlongident in
  { exp with exp_type = instance v.val_type;
    exp_desc = Texp_ident(p, v) }

let trx_mkcsp exp =
  let (p, v) = Lazy.force pathval_trx_mkcsp in
  { exp with exp_type = instance v.val_type;
    exp_desc = Texp_ident(p, v) }


let mkString exp s =
  { exp with 
    exp_type = instance_def Predef.type_string;
    exp_desc = Texp_constant(Const_string (s)) }

let mkInt exp i =
  { exp with
    exp_type = instance_def Predef.type_int;
    exp_desc = Texp_constant(Const_int (i))}

let quote_constant exp cst =
  let (constr,e) =
    match cst with
    | Const_int _ -> (Lazy.force constr_const_int, exp)
    | Const_char _ -> (Lazy.force constr_const_char, exp)
    | Const_string s -> (Lazy.force constr_const_string, mkString exp s)
    | Const_float s -> (Lazy.force constr_const_float, mkString exp s)
    | Const_int32 _ -> (Lazy.force constr_const_int32, exp)
    | Const_int64 _ -> (Lazy.force constr_const_int64, exp)
    | Const_nativeint _ -> (Lazy.force constr_const_nativeint, exp)
  in {exp with exp_type = Lazy.force type_constant;
      exp_desc = Texp_construct(constr, [e]) } 

(* Walid: We should factor out the 'mk' functionality. *)

let mkNone exp =
  { exp with exp_type = Lazy.force type_exp_option;
    exp_desc = Texp_construct(Lazy.force constr_none, []) }
    
let mkSome exp e =
  { exp with exp_type = Lazy.force type_exp_option;
    exp_desc = Texp_construct(Lazy.force constr_some, [e]) }

let mkPexpOption exp eo = match eo with
  None -> mkNone exp
| Some e -> mkSome exp e

let quote_rec_flag rf exp =
  let cst = match rf with
    Nonrecursive -> constr_nonrecursive
  | Recursive -> constr_recursive
  | Default -> constr_default
  in { exp with 
       exp_type = Lazy.force type_rec_flag;
       exp_desc = Texp_construct(Lazy.force cst, []) } 

let quote_direction_flag df exp =
  let cst = match df with
    Upto -> constr_upto
  | Downto -> constr_downto
  in { exp with exp_type = Lazy.force type_rec_flag;
       exp_desc = Texp_construct(Lazy.force cst, []) }

let mkfalse exp =
  { exp with exp_type = Lazy.force type_bool;
    exp_desc = Texp_construct(Lazy.force constr_false, []) }

let mktrue exp =
  { exp with exp_type = Lazy.force type_bool;
    exp_desc = Texp_construct(Lazy.force constr_true, []) } 


let mkExp exp t d = 
  { exp with exp_type = Lazy.force t;
    exp_desc = d}

let mkPat exp t d =
  { exp with pat_type = Lazy.force t;
    pat_desc = d}


let rec quote_longident exp li =
  match li with
    Longident.Lident s ->
      mkExp exp
        type_longident_t
        (Texp_construct(Lazy.force constr_longident_lident, 
                        [mkString exp s]))
  | Longident.Ldot (li',s) ->
      mkExp exp
        type_longident_t
        (Texp_construct(Lazy.force constr_longident_ldot,
                        [quote_longident exp li';
                         mkString exp s]))
  | Longident.Lapply (li1,li2) ->
      mkExp exp
        type_longident_t
        (Texp_construct(Lazy.force constr_longident_lapply,
                        [quote_longident exp li1;
                         quote_longident exp li2]))

let quote_label exp l =
  { exp with 
    exp_type = Lazy.force type_label;
    exp_desc = Texp_constant(Const_string (l)) }

let rec mkIdent exp id =
  match id with
    Longident.Lident s ->
      mkExp exp type_longident_t
        (Texp_construct(Lazy.force constr_longident_lident,
                        [mkString exp s]))
  |  Longident.Ldot (id', s) ->
      let exp' = mkIdent exp id' in
      mkExp exp type_longident_t
        (Texp_construct(Lazy.force constr_longident_ldot,
                        [exp'; mkString exp s]))
  |  Longident.Lapply (id1, id2) ->
      let exp1 = mkIdent exp id1 in
      let exp2 = mkIdent exp id2 in
      mkExp exp type_longident_t
        (Texp_construct(Lazy.force constr_longident_lapply,
                        [exp1;exp2]))

let rec quote_ident exp path =
  match path with
    Path.Pident i -> mkExp exp
        type_longident_t
        (Texp_construct(Lazy.force constr_longident_lident, 
                        [mkString exp (Ident.name i)]))
  | Path.Pdot (path',s,k) ->
      mkExp exp
        type_longident_t
        (Texp_construct(Lazy.force constr_longident_ldot,
                        [quote_ident exp path';
                         mkString exp s]))
  | Path.Papply (path1,path2) ->
      mkExp exp
        type_longident_t
        (Texp_construct(Lazy.force constr_longident_lapply,
                        [quote_ident exp path1;
                         quote_ident exp path2]))


let mkParseTree exp d =
  {exp with
   exp_desc =
   Texp_record([Lazy.force label_pexp_desc,
                mkExp exp type_parsetree_expression_desc d;
                Lazy.force label_pexp_loc,
                quote_location exp],
               None) }

let mkParsePattern exp d =
  mkExp exp
    type_parsetree_pattern
    (Texp_record([Lazy.force label_ppat_desc, mkExp exp type_parsetree_pattern_desc d;
                  Lazy.force label_ppat_loc, quote_location exp],
                 None))
    

let rec mkPexpList exp l =
  match l with
    [] ->    mkExp exp
        type_list
        (Texp_construct(Lazy.force constr_nil, 
                        []))
  | x::xs -> mkExp exp
        type_list
        (Texp_construct(Lazy.force constr_cons, 
                        [x;mkPexpList exp xs]))

let mkPexpTuple exp exps = 
  mkExp exp
    type_parsetree_expression_desc
    (Texp_construct(Lazy.force constr_pexp_tuple, exps))

let mkPpatTuple exp exps =
  mkExp exp
    type_parsetree_pattern_desc
    (Texp_construct(Lazy.force constr_ppat_tuple, 
                    [mkPexpList exp exps]))

let rec quote_list_as_expopt_forexps exp el =
  match el with
    [] -> mkNone exp
  | [e] -> mkSome exp e
  | _ -> mkSome exp
        (mkParseTree exp
           (Texp_construct(Lazy.force constr_pexp_tuple,
                                [mkPexpList exp el])))

let rec quote_list_as_expopt_forpats exp el =
  match el with
    [] -> mkNone exp
  | [e] -> mkSome exp e
  | _ -> mkSome exp
        (mkParsePattern exp
           (Texp_construct(Lazy.force constr_ppat_tuple,
                                [mkPexpList exp el])))

let gensymstring_count = ref 0

(* generates a fresh identifier *)
let gensymstring s =
  incr gensymstring_count;
  s ^ "_" ^ string_of_int !gensymstring_count

(* resets the counter used to ensure unique identifiers *)
let reset_gensymstring_counter () = gensymstring_count := 0

let gensymlongident li =
  match li with
    Longident.Lident s -> Longident.Lident (gensymstring s)
  | _ -> fatal_error ("Trx.gensymstring: not a simple id")

let longidenttostring li =
  match li with
    Longident.Lident s -> s
  | _ -> fatal_error ("Trx.longidenttostring: li is not a simple id")

let rec boundinpattern p l = (* extend list l with ids bound in pattern p *)
  match p.pat_desc with
    Tpat_any -> l
  | Tpat_var i -> add_ifnew i l
  | Tpat_alias (p,i) -> boundinpattern p (add_ifnew i l)
  | Tpat_constant c -> l
  | Tpat_tuple pl -> List.fold_right boundinpattern pl l
  | Tpat_construct (cd,pl) -> 
      List.fold_right boundinpattern pl l
  | Tpat_variant (_,po,_) -> (match po with
      None -> l
    | Some p -> boundinpattern p l)
  | Tpat_record dpl -> List.fold_right (fun (d,p) -> boundinpattern p) dpl l
  | Tpat_array pl -> List.fold_right boundinpattern pl l
  | Tpat_or (p1,p2,_) -> boundinpattern p2 (boundinpattern p1 l)
  | Tpat_lazy p -> boundinpattern p l

let rec mkPattern exp p =
  let idexp id = mkExp exp type_longident_t
      (Texp_ident (Path.Pident id,
                   {val_type = Lazy.force type_longident_t;
                    val_kind = Val_reg}))
  in let strexp id = mkExp exp (instance_def Predef.type_string)
      (Texp_apply (trx_longidenttostring exp, [(Some (idexp id),
                                                Required)]))
  in match p.pat_desc with 
    Tpat_any -> mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_any, 
                             []))
  | Tpat_var id -> mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_var, 
                             [strexp id]))
  | Tpat_alias (p,i) ->
      mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_alias,
                             [mkPattern exp p;
                              strexp i]))
  | Tpat_constant cst ->
      mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_constant, 
                             [quote_constant
                                {
                                 exp_desc = Texp_constant cst;
                                 exp_loc  = p.pat_loc;
                                 exp_type = p.pat_type;
                                 exp_env  = p.pat_env
                               } 
                                cst]))
  | Tpat_tuple pl ->
      let el = List.map (mkPattern exp) pl
      in mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_tuple, 
                             [mkPexpList exp el]))
  | Tpat_construct ({cstr_tag=tag},pl) ->
      let lid = get_constr_lid tag p.pat_type p.pat_env in
      mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_construct,
                             [quote_longident exp lid;
                              quote_list_as_expopt_forpats exp
                                (List.map (mkPattern exp) pl);
                              mkfalse exp ])) 
  | Tpat_variant (l,po,rd) ->
      mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_variant,
                             [mkString exp l;
                              mkPexpOption exp
                                (map_option (mkPattern exp) po)]))
  | Tpat_record dpil ->
      let lids = get_record_lids p.pat_type p.pat_env in
      let dpil = List.map
          (fun (d,p) -> (d,p,List.nth lids d.lbl_pos))
          dpil in
      let get_idpat =
        fun (d,p,lid) ->
          mkPexpTuple exp [quote_longident exp lid;
                           mkPattern exp p]
      in
      mkParsePattern exp
        (Texp_construct(Lazy.force constr_ppat_record,
                             [mkPexpList exp
                                (List.map get_idpat dpil)
                            ])) 
  | Tpat_array pl ->
      mkParsePattern exp 
        (Texp_construct(Lazy.force constr_ppat_array,
                             [mkPexpList exp
                                (List.map (mkPattern exp) pl)]))
  | Tpat_or (p1,p2,_) ->
      mkParsePattern exp 
        (Texp_construct(Lazy.force constr_ppat_or,
                             [mkPattern exp p1;
                              mkPattern exp p2]))
  | Tpat_lazy p ->
      mkParsePattern exp 
        (Texp_construct(Lazy.force constr_ppat_lazy,
                             [mkPattern exp p]))

let mkNewPEL exp pEl = 
  List.map (fun (p,e) -> mkPexpTuple exp [mkPattern exp p;e]) pEl


(* The preprocessing transformation proper *)


let call_trx_mkcsp exp v li =
  let exp' = {exp with exp_desc = Texp_cspval (Obj.magic v, Longident.parse "")}
  and expli = {exp with exp_desc = Texp_cspval (Obj.magic li, Longident.parse "")}
  in {exp with exp_desc = 
      (Texp_apply (trx_mkcsp exp, [(Some exp, Required);
                                   (Some exp',Required);
                                   (Some expli,Required)]))}

(* Postprocessing expressions at level n *)
let rec trx_e n exp =
    match exp.exp_desc with
      (* function is called at run time,so it gets compiled, if we can keep the information at this point *)
      Texp_ident (i,vd) ->
	let stage = 
	  try Env.find_stage i exp.exp_env
	  with Not_found ->
	    ignore(Warnings.print Format.err_formatter 
	      (Warnings.Camlp4 ("Stage for var is set to implicit 0:" ^ 
	       Path.name i ^ "\n")));
	    [] in
        if stage = [] then              (* or stage < n? We would typecheck again*)
            let _ = Env.make_env_pure exp.exp_env in
            let _ = Env.update_ident_timestamp exp.exp_env in
            let v = (Some {exp with exp_type = instance vd.val_type})
            in call_trx_mkcsp exp v (path_to_lid i)
        else
            mkParseTree exp   (* construct VAR x *)
              (Texp_construct(Lazy.force constr_pexp_ident, 
                 [{exp with exp_type = Lazy.force type_longident_t}]))
          

    | Texp_let (rf, pel, e1) ->
        begin
          match rf with
            Recursive ->
              let idlist = List.fold_right (fun (p,e) -> boundinpattern p) pel []
              and gensymexp id =  (* (gensym "x") *)
                mkExp exp
                  type_longident_t
                  (Texp_apply
                     (trx_gensymlongident exp,
                      [(Some (quote_ident exp (Path.Pident id)),
                        Required)]))
              and idpat id =
                {pat_desc = Tpat_var id;
                 pat_loc = exp.exp_loc;
                 pat_type = Lazy.force type_longident_t;
                 pat_env = exp.exp_env}
              and translet =
                mkParseTree exp
                  (Texp_construct
                     (Lazy.force constr_pexp_let, 
                      [quote_rec_flag rf exp;
                       mkPexpList exp 
                         (mkNewPEL exp
                            (List.map (map_pi2 (trx_e n)) pel));
                       trx_e n e1
                     ]
                     )
                  )
              in let pel' = List.map (fun id -> (idpat id, gensymexp id)) idlist
              in mkExp exp
                type_parsetree_expression
                (Texp_let
                   (Nonrecursive,
                    pel', 
                    translet))


          | _ ->
              let idlist = List.fold_right (fun (p,e) -> boundinpattern p) pel []
              and peil = let genid () = Ident.create (gensymstring "fresh")
              in List.map (fun (p,e) -> (p,e, genid())) pel
              and gensymexp id =  (* (gensym "x") *)
                mkExp exp
                  type_longident_t
                  (Texp_apply
                     (trx_gensymlongident exp,
                      [(Some (quote_ident exp (Path.Pident id)),
                        Required)]))
              in let idpat_t id t =
                {pat_desc = Tpat_var id;
                 pat_loc = exp.exp_loc;
                 pat_type = Lazy.force t;
                 pat_env = exp.exp_env}
              in let idpat id = idpat_t id type_longident_t
              in let idexp e id =
                {e with exp_desc =
                 (Texp_ident (Path.Pident id,
                              {val_type = e.exp_type;
                               val_kind = Val_reg}))}
              in let pel' = List.map (fun (p,e,i) -> (p, idexp e i)) peil
              in let translet =
                mkParseTree exp
                  (Texp_construct
                     (Lazy.force constr_pexp_let, 
                      [quote_rec_flag rf exp;
                       mkPexpList exp (mkNewPEL exp pel');
                       trx_e n e1
                     ]
                     )
                  )
              in let pel1 = List.map (fun id -> (idpat id, gensymexp id)) idlist
              in let pel2 = List.map
                  (fun (p,e,i) -> (idpat_t i type_parsetree_expression,
                                   trx_e n e))
                  peil
              in mkExp exp
                type_parsetree_expression
                (Texp_let
                   (Nonrecursive,
                    List.append pel1 pel2, 
                    translet))
        end

    | Texp_function (pel, partial) ->
        (* XXO implement the translation
           trans(\x.e)  --->  let x = gensym "x" in LAM x trans(e) *)
        let idlist =
          List.fold_right (fun (p,e) -> boundinpattern p) pel []  
        and gensymexp id =  (* (gensym "x") *)
          mkExp exp
            type_longident_t
            (Texp_apply
               (trx_gensymlongident exp,
                [(Some (quote_ident exp (Path.Pident id)),
                  Required)]))
        and idpat id =
          {pat_desc = Tpat_var id;
           pat_loc = exp.exp_loc;
           pat_type = Lazy.force type_longident_t;
           pat_env = exp.exp_env}
        and transfunction =   (* LAM x trans(e) *)
          mkParseTree exp
            (Texp_construct
               (Lazy.force constr_pexp_function, 
                [mkString exp "";
                 mkNone exp;
                 mkPexpList exp 
                   (mkNewPEL exp
                      (List.map (map_pi2 (trx_e n)) pel))
               ]
               )
            )
        in let pel' = List.map (fun id -> (idpat id, gensymexp id)) idlist
        in mkExp exp
          type_parsetree_expression
          (Texp_let
             (Nonrecursive,
              pel', 
              transfunction))
          
(* XXO the following is a bit hoky.  We also don't really put
   the real type for lists right now.  We need to ask a higher
   power.  Walid.  *)
    | Texp_apply (e,eool) ->
        let eol = List.map fst eool in
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_apply, 
                               [trx_e n e;
                                mkPexpList exp (map_strict (fun x -> mkPexpTuple exp
                                    [mkString exp "";
                                     trx_e n x]) 
                                                  eol)]))
    | Texp_match (e,pel,partial) ->
        let idlist =
          List.fold_right (fun (p,e) -> boundinpattern p) pel []  
        and gensymexp id =  (* (gensym "x") *)
          mkExp exp
            type_longident_t
            (Texp_apply
               (trx_gensymlongident exp,
                [(Some (quote_ident exp (Path.Pident id)),
                  Required)]))
        and idpat id =
          {pat_desc = Tpat_var id;
           pat_loc = exp.exp_loc;
           pat_type = Lazy.force type_longident_t;
           pat_env = exp.exp_env}
        and transmatch =
          mkParseTree exp
            (Texp_construct
               (Lazy.force constr_pexp_match, 
                [trx_e n e;
                 mkPexpList exp 
                   (mkNewPEL exp
                      (List.map (map_pi2 (trx_e n)) pel))
               ]
               )
            )
        in let pel' = List.map (fun id -> (idpat id, gensymexp id)) idlist
        in mkExp exp
          type_parsetree_expression
          (Texp_let
             (Nonrecursive,
              pel', 
              transmatch))
          
    | Texp_try (e,pel) ->
        let idlist =
          List.fold_right (fun (p,e) -> boundinpattern p) pel []  
        and gensymexp id =  (* (gensym "x") *)
          mkExp exp
            type_longident_t
            (Texp_apply
               (trx_gensymlongident exp,
                [(Some (quote_ident exp (Path.Pident id)),
                  Required)]))
        and idpat id =
          {pat_desc = Tpat_var id;
           pat_loc = exp.exp_loc;
           pat_type = Lazy.force type_longident_t;
           pat_env = exp.exp_env}
        and transtry =
          mkParseTree exp
            (Texp_construct
               (Lazy.force constr_pexp_try, 
                [trx_e n e;
                 mkPexpList exp 
                   (mkNewPEL exp
                      (List.map (map_pi2 (trx_e n)) pel))
               ]
               )
            )
        in let pel' = List.map (fun id -> (idpat id, gensymexp id)) idlist
        in mkExp exp
          type_parsetree_expression
          (Texp_let
             (Nonrecursive,
              pel', 
              transtry))

    | Texp_tuple el ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_tuple,
                               [mkPexpList exp (List.map (trx_e n) el)]))
    | Texp_construct ({cstr_tag=tag}, el) ->
        let lid = get_constr_lid tag exp.exp_type exp.exp_env in
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_construct,
                          [quote_longident exp lid;
                           quote_list_as_expopt_forexps exp
                             (List.map (trx_e n) el);
                           mkfalse exp])) 
    | Texp_variant (label, eo) ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_variant,
                               [mkString exp label;
                                mkPexpOption exp (map_option (trx_e n) eo)]))      
    | Texp_record (del, eo) ->
        let lids = get_record_lids exp.exp_type exp.exp_env in
        let idel = List.map (fun (d,e) -> (List.nth lids d.lbl_pos, d, e)) del in
(* Walid: why "mkPexpTuple exp" instead of "mkPexpTuple e" *)
        let mklidexp (lid,d,e) =
          mkPexpTuple exp 
            [quote_longident exp lid;
             trx_e n e]
        in let iel = List.map mklidexp idel
        in mkParseTree exp 
          (Texp_construct(Lazy.force constr_pexp_record,
                               [mkPexpList exp iel;
                                mkPexpOption exp (map_option (trx_e n) eo)]))
    | Texp_field (e,ld) ->
        let lids = get_record_lids e.exp_type e.exp_env in
        let lid = List.nth lids ld.lbl_pos in
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_field,
                               [trx_e n e;
                                quote_longident exp lid]))

    | Texp_setfield (e1,ld,e2) ->
        let lids = get_record_lids e1.exp_type e1.exp_env in
        let lid = List.nth lids ld.lbl_pos in
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_setfield,
                               [trx_e n e1;
                                quote_longident exp lid;
                                trx_e n e2]))
    | Texp_array el ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_array,
                               [mkPexpList exp (List.map (trx_e n) el)]))
    | Texp_ifthenelse (e1,e2,eo) ->
        mkParseTree  exp
          (Texp_construct(Lazy.force constr_pexp_ifthenelse,
                               [trx_e n e1;
                                trx_e n e2;
                                mkPexpOption exp (map_option (trx_e n) eo)]))
    | Texp_sequence (e1,e2) ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_sequence,
                               [trx_e n e1;
                                trx_e n e2]))
    | Texp_while (e1,e2) ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_while,
                               [trx_e n e1;
                                trx_e n e2]))
    | Texp_for (id,e1,e2,df,e3) ->
        let gensymexp id =  (* (gensym "x") *)
          mkExp exp
            type_longident_t
            (Texp_apply
               (trx_gensymlongident exp,
                [(Some (quote_ident exp (Path.Pident id)),
                  Required)]))
        and idpat id =
          {pat_desc = Tpat_var id;
           pat_loc = exp.exp_loc;
           pat_type = Lazy.force type_longident_t;
           pat_env = exp.exp_env}
        and idexp id =
          mkExp exp
            type_longident_t
            (Texp_ident (Path.Pident id,
                         {val_type = Lazy.force type_longident_t;
                          val_kind = Val_reg}))
        in let strexp id =
          mkExp exp
            (instance_def Predef.type_string)
            (Texp_apply (trx_longidenttostring exp, [(Some (idexp id),
                                                      Required)]))
        in let transfor =
          mkParseTree exp
            (Texp_construct(Lazy.force constr_pexp_for, 
                                 [strexp id;
                                  trx_e n e1;
                                  trx_e n e2; 
                                  quote_direction_flag df exp;
                                  trx_e n e3;]))
        in mkExp exp
          type_parsetree_expression
          (Texp_let
             (Nonrecursive,
              [(idpat id, gensymexp id)], 
              transfor))

    | Texp_when (e1,e2) ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_when,
            [trx_e n e1;
             trx_e n e2]))
    | Texp_send (e,m) ->
        let s = match m with
        |  Tmeth_name s -> s
        |  Tmeth_val i -> Ident.name i in
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_send,
                               [trx_e n e;
                                mkString exp s]))
    | Texp_new (p,cd) ->
	check_path_quotable p;
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_new,
                               [quote_ident exp p]))
          (* similar to Texp_for *)
    | Texp_assert e ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_assert, 
                               [trx_e n e]))
    | Texp_lazy e ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_lazy, 
                               [trx_e n e]))

    | Texp_bracket e ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_bracket, 

    | Texp_escape e ->
        if n=1 then
          trx_e (n-1) e
        else
          mkParseTree exp
            (Texp_construct(Lazy.force constr_pexp_escape, 
                                 [trx_e (n-1) e]))
    | Texp_run e ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_run, 
                               [trx_e n e]))
  end
*)

(* ------------------------------------------------------------------------ *)
(* The main function to translate away brackets. It receives
   an expression at the level n > 0.
*)

(* Given a type [ty], return [ty code code ... code] (n times code).
   When we push the bracket in, expressions that had type ty before
   will have the type ('cl,ty) code.
   Here, ('cl,ty) code is an abtract type whose concrete representation
   is Parsetree.
   Generally speaking we don't have to adjust the types since the
   type checking is finished. However, code generator may look
   at types; it's better if we don't lie. Thus, as trx_bracket
   translates the expression, it should also adjust the types.
*)

let rec wrap_ty_in_code : int -> type_expr -> type_expr = fun n ty ->
  if n=0 then ty else
  let clsfier = Btype.newgenvar () in
  wrap_ty_in_code (n-1) (Predef.type_code clsfier ty)



let not_supported msg =
  raise (TrxError (msg ^ " is not yet supported within brackets"))



let rec trx_bracket : 
  (expression -> expression) -> (* 0-level traversal *)
  int -> (expression -> expression) = fun trx_exp n exp ->
  let new_desc = match exp.exp_desc with
    (* Don't just do when vd.val_kind = Val_reg 
       because (+) or Array.get are Val_prim *)
  | Texp_ident (p,li,vd)  ->
    let stage = try Env.find_stage p exp.exp_env
	        with Not_found ->
	           ignore(Warnings.print Format.err_formatter 
	           (Warnings.Camlp4 ("Stage for var is set to implicit 0:" ^ 
	           Path.name p ^ "\n")));  [] in
    (* We make CSP only if the variable is bound at the stage 0.
       Variables bound at stage > 0 are subject to renaming.
       They are translated into stage 0 variable but of a different
       type (Longident.t loc), as explained in the title comments.
       We also do the non-escaping check.
     *)
    if stage = [] then trx_csp exp p li 
    else                                (* XXX *)
      let ast = 
        {pexp_loc = exp.exp_loc;
         pexp_desc = Pexp_ident (Location.mkloc (path_to_lid p) li.loc)}
      in Texp_cspval (Obj.repr ast, dummy_lid "*id*")
  | Texp_constant cst ->
    let ast = 
      {pexp_loc = exp.exp_loc;
       pexp_desc = Pexp_constant cst}
    in Texp_cspval (Obj.repr ast, dummy_lid "*cst*")
  | Texp_instvar (p1,p2,s) ->
     not_supported "Objects (Texp_instvar)"
        (* Alternatively: since instance variables are always bound 
           at level 0 (for now)
           so this is like a csp variable 
        call_trx_mkcsp exp None (path_to_lid p2)
        *)
  | Texp_setinstvar _ -> not_supported "Objects (Texp_setinstvar)"
  | Texp_override  _  -> not_supported "Objects (Texp_override)"
  | Texp_letmodule (id,s,me,e) -> not_supported "let module"

  | Texp_assert e ->
      let loc_exp = texp_ident "Trx.sample_loc" in (* this fills in the type, etc.*)
      let loc_exp = 
      {loc_exp with exp_desc = Texp_cspval (Obj.repr exp.exp_loc, 
                                            dummy_lid "*loc*")}
      in
      texp_apply (texp_ident "Trx.build_assert")
        [loc_exp; trx_bracket trx_exp n e]
  | Texp_assertfalse ->
    let ast = 
      {pexp_loc = exp.exp_loc;
       pexp_desc = Pexp_assertfalse}
    in Texp_cspval (Obj.repr ast, dummy_lid "*af*")
  | Texp_object (cl,fl) -> not_supported "Objects"
  | Texp_pack _         -> not_supported "First-class modules"

  | Texp_cspval (v,li) ->               (* CSP is a sort of a constant *)
    let ast = 
      {pexp_loc = exp.exp_loc;
       pexp_desc = Pexp_cspval(v,li)}
    in Texp_cspval (Obj.repr ast, dummy_lid "*csp*")
  | _ -> failwith "not yet implemented"
  in
  {exp with exp_type = wrap_ty_in_code n exp.exp_type;
            exp_desc = new_desc}


(* ------------------------------------------------------------------------ *)
(* Typedtree traversal to eliminate bracket/escapes *)

(* Functions to help traverse and transform a tree.
   We assume that every tree mapping function of the type 'a -> 'a
   throws the exception Not_modified if the tree has not been
   modified.
   This protocol helps minimize garbage and prevent useless tree
   duplication.
*)

exception Not_modified

let replace_list : ('a -> 'a) -> 'a list -> 'a list = fun f l ->
  let rec loop mdf = function
  | [] -> if mdf then [] else raise Not_modified
  | h::t -> match (try Some (f h) with Not_modified -> None) with
             | Some h -> h :: loop true t
             | None   -> h :: loop mdf  t
  in loop false l

let replace_pair : ('a -> 'a) -> ('b -> 'b) -> 'a * 'b -> 'a * 'b =
  fun f g (x,y) ->
  match ((try Some (f x) with Not_modified -> None),
         (try Some (g y) with Not_modified -> None)) with
  | (None,None)      -> raise Not_modified
  | (Some x, None)   -> (x,y)
  | (None, Some y)   -> (x,y)
  | (Some x, Some y) -> (x,y)

let replace_opt : ('a -> 'a) -> 'a option -> 'a option = fun f -> function
  | Some e -> Some (f e)
  | None   -> raise Not_modified

(* The main function to scan the typed tree at the 0 level and
   detect brackets 
*)

let rec trx_struct str =
  {str with str_items = 
  replace_list (fun si -> {si with str_desc = trx_struct_item si.str_desc})
           str.str_items}

and trx_struct_item = function
| Tstr_eval e -> Tstr_eval (trx_exp e)
| Tstr_value (rf,pel) ->
    Tstr_value(rf, replace_list (fun (p,e) -> (p, trx_exp e)) pel)
| Tstr_primitive (_,_,_) 
| Tstr_type _
| Tstr_exception (_,_,_)
| Tstr_exn_rebind (_,_,_,_) -> raise Not_modified
| Tstr_module (i,l,me) -> Tstr_module (i, l, trx_me me)
| Tstr_recmodule l ->
  Tstr_recmodule (replace_list (fun (i,l,mt,me) -> (i,l,mt,trx_me me)) l)
| Tstr_modtype (_,_,_)
| Tstr_open (_,_) -> raise Not_modified
| Tstr_class l ->
    Tstr_class (replace_list (fun (dcl,sl,vf) -> (trx_cdcl dcl,sl,vf)) l)
| Tstr_class_type _ -> raise Not_modified
| Tstr_include (me,il) -> Tstr_include (trx_me me, il)

and trx_me me = 
  {me with mod_desc = trx_me_desc me.mod_desc} 

and trx_me_desc = function
| Tmod_ident _ -> raise Not_modified
| Tmod_structure str -> Tmod_structure (trx_struct str)
| Tmod_functor (i,l,t,me) -> Tmod_functor (i,l,t, trx_me me)
| Tmod_apply (me1,me2,mc) ->
  let (me1,me2) = replace_pair trx_me trx_me (me1,me2) in
  Tmod_apply (me1, me2, mc)
| Tmod_constraint (me,mt,mtc,mc) -> Tmod_constraint (trx_me me, mt, mtc, mc)
| Tmod_unpack (e,mt) -> Tmod_unpack (trx_exp e,mt)

and trx_cdcl class_decl =
  {class_decl with ci_expr = trx_ce class_decl.ci_expr}

and trx_ce class_expr =
  {class_expr with cl_desc = trx_ce_desc class_expr.cl_desc}

and trx_cl_struct cs =
  {cs with cstr_fields = 
     replace_list (fun cf -> {cf with cf_desc = trx_cf cf.cf_desc})
                  cs.cstr_fields}

and trx_ce_desc = function
| Tcl_ident (_,_,_) -> raise Not_modified
| Tcl_structure cs ->
  Tcl_structure (trx_cl_struct cs)
| Tcl_fun (l,p,el,ce,pa) ->
  let (el,ce) = 
        replace_pair (replace_list (fun (i,l,e) -> (i,l,trx_exp e)))
                     trx_ce (el,ce) in
  Tcl_fun (l,p,el,ce,pa)
| Tcl_apply (ce,el) ->
  let repel (l,eo,o) = (l,replace_opt trx_exp eo,o) in
  let (ce,el) = replace_pair trx_ce (replace_list repel) (ce,el) in
  Tcl_apply (ce,el)
| Tcl_let (rf,el1,el2,ce) ->
  let repel1 = replace_list (fun (p,e) -> (p,trx_exp e)) in
  let repel2 = replace_list (fun (i,l,e) -> (i,l,trx_exp e)) in
  let ((el1,el2),ce) = replace_pair (replace_pair repel1 repel2) trx_ce
                        ((el1,el2),ce)
  in Tcl_let (rf,el1,el2,ce)
| Tcl_constraint (ce,ct,sl1,sl2,cty) ->
  Tcl_constraint (trx_ce ce,ct,sl1,sl2,cty)

and trx_cf = function
| Tcf_inher (ofl,ce,so,sl1,sl2) ->
  Tcf_inher (ofl,trx_ce ce,so,sl1,sl2)
| Tcf_val (_,_,_,_,Tcfk_virtual _,_) -> raise Not_modified
| Tcf_val (s,l,mf,i,Tcfk_concrete e,b) ->
  Tcf_val (s,l,mf,i,Tcfk_concrete (trx_exp e),b)
| Tcf_meth (s,l,pf,Tcfk_virtual _,_) -> raise Not_modified
| Tcf_meth (s,l,pf,Tcfk_concrete e,b) ->
  Tcf_meth (s,l,pf,Tcfk_concrete (trx_exp e),b)
| Tcf_constr (_,_) -> raise Not_modified
| Tcf_init e -> Tcf_init (trx_exp e)

and trx_exp exp =
  {exp with exp_desc = trx_expression exp.exp_desc}

and trx_pelist l = replace_list (fun (p,e) -> (p,trx_exp e)) l
and trx_expression = function
| Texp_ident (_,_,_)
| Texp_constant _ -> raise Not_modified
| Texp_let (rf, el, e) ->
  let (el,e) = replace_pair trx_pelist trx_exp (el,e)
  in Texp_let (rf, el, e)
| Texp_function (l,el,p) ->
  Texp_function (l,trx_pelist el,p)
| Texp_apply (e,el) ->
  let repl (l,eo,op) = (l,replace_opt trx_exp eo,op) in
  let (e,el) = replace_pair trx_exp (replace_list repl) (e,el)
  in Texp_apply (e,el)
| Texp_match (e,el,p) ->
  let (e,el) = replace_pair trx_exp trx_pelist (e,el)
  in Texp_match (e,el,p)
| Texp_try (e,el) ->
  let (e,el) = replace_pair trx_exp trx_pelist (e,el)
  in Texp_try (e,el)
| Texp_tuple l -> Texp_tuple (replace_list trx_exp l)
| Texp_construct (p,l,cd,el,b) ->
  Texp_construct (p,l,cd,replace_list trx_exp el,b)
| Texp_variant (l,eo) -> Texp_variant (l,replace_opt trx_exp eo)
| Texp_record (ll,eo) ->
  let repll (p,l,ld,e) = (p,l,ld,trx_exp e) in
  let (ll,eo) = replace_pair (replace_list repll) (replace_opt trx_exp) (ll,eo)
  in Texp_record (ll,eo)
| Texp_field (e,p,l,ld) -> Texp_field (trx_exp e,p,l,ld)
| Texp_setfield (e1,p,l,ld,e2) ->
  let (e1,e2) = replace_pair trx_exp trx_exp (e1,e2)
  in Texp_setfield (e1,p,l,ld,e2)
| Texp_array el -> Texp_array (replace_list trx_exp el)
| Texp_ifthenelse (e1,e2,eo) ->
  let ((e1,e2),eo) = replace_pair (replace_pair trx_exp trx_exp) 
                                  (replace_opt trx_exp) ((e1,e2),eo)
  in Texp_ifthenelse (e1,e2,eo)
| Texp_sequence (e1,e2) -> 
  let (e1,e2) = replace_pair trx_exp trx_exp (e1,e2)
  in Texp_sequence (e1,e2)
| Texp_while (e1,e2) ->
  let (e1,e2) = replace_pair trx_exp trx_exp (e1,e2)
  in Texp_while (e1,e2)
| Texp_for (i,l,e1,e2,df,e3) ->
  let ((e1,e2),e3) = replace_pair (replace_pair trx_exp trx_exp) 
                                  trx_exp ((e1,e2),e3)
  in Texp_for (i,l,e1,e2,df,e3)
| Texp_when (e1,e2) ->
  let (e1,e2) = replace_pair trx_exp trx_exp (e1,e2)
  in Texp_when (e1,e2)
| Texp_send (e1,m,eo) ->
  let (e1,eo) = replace_pair trx_exp (replace_opt trx_exp) (e1,eo)
  in Texp_send (e1,m,eo)
| Texp_new (_,_,_)
| Texp_instvar (_,_,_) -> raise Not_modified
| Texp_setinstvar (p1,p2,l,e) -> Texp_setinstvar (p1,p2,l,trx_exp e)
| Texp_override (p, el) ->
  Texp_override (p, replace_list (fun (p,l,e) -> (p,l,trx_exp e)) el)
| Texp_letmodule (i,l,me,e) ->
  let (me,e) = replace_pair trx_me trx_exp (me,e)
  in Texp_letmodule (i,l,me,e)
| Texp_assert e -> Texp_assert (trx_exp e)
| Texp_assertfalse -> raise Not_modified
| Texp_lazy e -> Texp_lazy (trx_exp e)
| Texp_object (cs,sl) -> Texp_object (trx_cl_struct cs,sl)
| Texp_pack me -> Texp_pack (trx_me me)

| Texp_bracket e -> (trx_bracket trx_exp 1 e).exp_desc

| Texp_escape _ -> assert false         (* Not possible in well-typed code *)
| Texp_run e -> 
    texp_apply (texp_ident "Runcode.run'") [trx_exp e]
| Texp_cspval (_,_) -> raise Not_modified


(* public interface *)
let trx_structure str = 
  try trx_struct str with Not_modified -> str

  
(* Obsolete: we never quite handled modules within the code

and quote_me n exp me = match me.mod_desc waith
| Tmod_structure str -> (* @@@@ *)
    mkParseModuleExpr exp
      (Texp_construct(Lazy.force constr_pmod_structure,
                           [quote_structure n exp str]))
| _ -> fatal_error "Trx.quote_me: case not implemented yet"

and quote_structure n exp str =
  mkPexpList exp (List.map (quote_structure_item n exp) str)

and quote_structure_item n exp si = match si with
| Tstr_value (rf,pel) ->  (* similar to texp_let *)
    begin
      match rf with
        Recursive ->
          let idlist = List.fold_right (fun (p,e) -> boundinpattern p) pel []
          and gensymexp id =  (* (gensym "x") *)
            mkExp exp
              type_longident_t
              (Texp_apply
                 (trx_gensymlongident exp,
                  [(Some (quote_ident exp (Path.Pident id)),
                    Required)]))
          and idpat id =
            {pat_desc = Tpat_var id;
             pat_loc = exp.exp_loc;
             pat_type = Lazy.force type_longident_t;
             pat_env = exp.exp_env}
          and translet =
            mkParseStructureItem exp
              (Texp_construct
                 (Lazy.force constr_pstr_value, 
                  [quote_rec_flag rf exp;
                   mkPexpList exp 
                     (mkNewPEL exp
                        (List.map (map_pi2 (trx_e n)) pel))
                 ]
                 )
              )
          in let pel' = List.map (fun id -> (idpat id, gensymexp id)) idlist
          in mkExp exp
            type_parsetree_expression
            (Texp_let
               (Nonrecursive,
                pel', 
                translet))
      | _ ->
          let idlist = List.fold_right (fun (p,e) -> boundinpattern p) pel []
          and peil = let genid () = Ident.create (gensymstring "fresh")
          in List.map (fun (p,e) -> (p,e, genid())) pel
          and gensymexp id =  (* (gensym "x") *)
            mkExp exp
              type_longident_t
              (Texp_apply
                 (trx_gensymlongident exp,
                  [(Some (quote_ident exp (Path.Pident id)),
                    Required)]))
          in let idpat_t id t =
            {pat_desc = Tpat_var id;
             pat_loc = exp.exp_loc;
             pat_type = Lazy.force t;
             pat_env = exp.exp_env}
          in let idpat id = idpat_t id type_longident_t
          in let idexp e id =
            {e with exp_desc =
             (Texp_ident (Path.Pident id,
                          {val_type = e.exp_type;
                           val_kind = Val_reg}))}
          in let pel' = List.map (fun (p,e,i) -> (p, idexp e i)) peil
          in let translet =
            mkParseStructureItem exp
              (Texp_construct
                 (Lazy.force constr_pstr_value, 
                  [quote_rec_flag rf exp;
                   mkPexpList exp (mkNewPEL exp pel')
                 ]
                 )
              )
          in let pel1 = List.map (fun id -> (idpat id, gensymexp id)) idlist
          in let pel2 = List.map
              (fun (p,e,i) -> (idpat_t i type_parsetree_expression,
                               trx_e n e))
              peil
          in mkExp exp
            type_parsetree_expression
            (Texp_let
               (Nonrecursive,
                List.append pel1 pel2, 
                translet))
    end
| _ -> fatal_error "Trx.quote_structure_item: case not implemented yet"

let mkParseModuleExpr exp d =
  mkExp exp
    type_parsetree_module_expr
    (Texp_record([Lazy.force label_pmod_desc,
                  mkExp exp type_parsetree_module_expr_desc d;
                  Lazy.force label_pmod_loc, quote_location exp],
                 None))

let type_parsetree_module_expr_desc = lazy (find_type "Parsetree.module_expr_desc")
let type_parsetree_module_expr = lazy (find_type "Parsetree.module_expr")

let label_pmod_desc = lazy (find_label "Parsetree.pmod_desc")
let label_pmod_loc  = lazy (find_label "Parsetree.pmod_loc")
let constr_pmod_structure     = lazy (find_constr "Parsetree.Pmod_structure")
let constr_pstr_value         = lazy (find_constr "Parsetree.Pstr_value")

let mkParseStructureItem exp d =
  mkExp exp
    type_parsetree_structure_item
    (Texp_record([Lazy.force label_pstr_desc,
                  mkExp exp type_parsetree_structure_item_desc d;
                  Lazy.force label_pstr_loc, quote_location exp],
                 None))

let label_pstr_desc = lazy (find_label "Parsetree.pstr_desc")


*)

(* Native mode is moved out to the `userland'

let native_mode = ref false (* ZZZ
  Should be a better way to detect native mode,
  e.g., by the presence of some modules in asmcomp *)

let remove_texp_cspval exp =
  if !native_mode = false then exp else
  failwith "native mode CSP are not impemented yet"

   XXX old code
  match exp.exp_desc with
  | Texp_cspval (v,l) ->
      let i = add_csp_value (v,l) in
      let exp' = {exp with exp_desc = Texp_constant (Const_int i)} in
      let desc = if !initial_native_compilation
        then (Texp_apply (trx_array_get exp, [(Some !local_csp_arr_texp, Required);(Some exp', Required)]))
	else (Texp_apply (trx_get_csp_value exp, [(Some exp', Required)])) in
      {exp with exp_desc = desc}
  | _ -> assert false

let trx_execute_expression exp =
  let (p, v) = Lazy.force pathval_trx_execute_expression in
  { exp with exp_type = instance v.val_type;
    exp_desc = Texp_ident(p, v) }
*)
