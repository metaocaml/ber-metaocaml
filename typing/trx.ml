open Parsetree
open Misc
open Asttypes
open Ctype
open Types
open Typedtree
open Parmatch
open Path
open Ident
open Env
open Typecore

(*
  The goal of this file is to post-process the Typedtree
  after the type checking and before the code generation to
  get rid of bracket, esc and run. The main function is
  trx_structure, which initiates the traversal and
  transforms every found expression with trx_e. The real
  transformation is done by trx_e.

  For example,
  <succ 1> gets transformed to mkApp <succ> <1> and eventually to
  mkApp (mkIdent "succ") (mkConst 1)
  (one may say that we `push brackets inside')

  So we replace bracket with calls to functions thayt construct
  Parsetree, which is the representation of code type.

  In terms of trees:
  After the type checking, <1> is represented as
  Texp_bracket (Texp_constant (Constant_int 1))
  We transform it to
  Texp_apply (Texp_ident "mkConstant") []

Future-stage identifier x was represented in tree as 
Texp_ident (ident,vd)
We transform x if it was written Pexp_ident li, or
in tree terms
Texp_construct ("Pexp_ident", [

*)


(* BER MetaOCaml version string *)
let meta_version  = "N 100"

exception TrxError of string

(* Path utilities *)
(* We always use path when available, and convert it to Longident
   when needed -- even if the Typedtree already carries the longident.
   The path is preferred because it is fully qualified for
   external identifiers and it is unambiguous.
   If we open a module, its components can be referred to without
   qualification -- but the path will be qualified.
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
   have the name that we can put into AST.
   Local names can't be put into AST since the type env in which
   they are declared is not represented in the AST.
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

(* Convert the path to an identifier. Since the path is assumed to be
   `global', time stamps don't matter and we can use just strings.
*)
let rec path_to_lid : Path.t -> Longident.t = function
  | Path.Pident i       -> Longident.Lident (Ident.name i)
  | Path.Pdot (p,s,_)   -> Longident.Ldot (path_to_lid p, s)
  | Path.Papply (p1,p2) ->
      Longident.Lapply(path_to_lid p1, path_to_lid p2)

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


(* Given a value and its type, create a constant (that is, literal value)
   of that type. For example, convert 1 to "1"
*)

let reify_to_literal v exp deflt =
  let exp_base_type =
    let exp_ty =
      Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
    (match Ctype.repr exp_ty with
    | {desc = Tconstr(p, _, _)} -> Some p
    | _ -> None) in
  match (Obj.is_int v, exp_base_type) with
  | (true,Some p) when Path.same p Predef.path_int ->
      Pexp_constant (Const_int (Obj.magic v))
  | (true,Some p) when Path.same p Predef.path_char ->
      Pexp_constant (Const_char (Obj.magic v))
  | (true,Some p) when Path.same p Predef.path_bool ->
      let b = if (Obj.magic v) then "true" else "false"
      in Pexp_construct (Longident.Lident b, None, false)
  | (false,_) when Obj.tag v = Obj.double_tag ->
      Pexp_constant (Const_float (string_of_float (Obj.magic v)))
  | (false,_) when Obj.tag v = Obj.string_tag ->
      Pexp_constant (Const_string (Obj.magic v))
  | (_,Some p) when Path.same p Predef.path_nativeint ->
      Pexp_constant (Const_nativeint (Obj.magic v))
  | (_,Some p) when Path.same p Predef.path_int32 ->
      Pexp_constant (Const_int32 (Obj.magic v))
  | (_,Some p) when Path.same p Predef.path_int64 ->
      Pexp_constant (Const_int64 (Obj.magic v))
  | _ -> deflt ()


(* ZZZ There is a lot of hacks here. First of all, mutation of rdesc
   is ugly.
   Second, we should add more constants,
   literal lists and perhaps even datatype constructors, which are global.
   At the very least [] should be literal, and short lists and arrays.
   Also, see my article about gprint and the drawback of mkcsp mentioned
   there, and correspondence with Cristiano about that time.
*)
let mkcsp v eo li =
  let deflt = Pexp_cspval (Obj.magic v, li) in
  let rdesc = match eo with
  | None -> deflt
  | Some exp ->				(* Try to make a literal AST value *)
      reify_to_literal v exp 		(* (pass by literal value) *)
      (fun () ->			(* Try to pass by reference *)
	match exp.exp_desc with
	| Texp_ident (i,_) when ident_can_be_quoted i ->
          Pexp_ident (path_to_lid i)
	| _ -> deflt)
  in {pexp_desc = rdesc;
      pexp_loc = Location.none}


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

let type_constant = function
    Const_int _ -> instance_def Predef.type_int
  | Const_char _ -> instance_def Predef.type_char
  | Const_string _ -> instance_def Predef.type_string
  | Const_float _ -> instance_def Predef.type_float
  | Const_int32 _ -> instance_def Predef.type_int32
  | Const_int64 _ -> instance_def Predef.type_int64
  | Const_nativeint _ -> instance_def Predef.type_nativeint

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

let pathval_run_expression = lazy (find_value "Runcode.run'")
let pathval_trx_longidenttostring = lazy (find_value "Trx.longidenttostring")
let pathval_trx_gensymlongident = lazy (find_value "Trx.gensymlongident")
let pathval_trx_mkcsp = lazy (find_value "Trx.mkcsp")

let run_expression exp =
  let (p, v) = Lazy.force pathval_run_expression in
  { exp with exp_type = instance v.val_type;
    exp_desc = Texp_ident(p, v) }

  
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

let quote_position exp p =
  {exp with exp_desc =
   Texp_record([Lazy.force label_pos_fname,
                mkString exp p.Lexing.pos_fname;
                Lazy.force label_pos_lnum,
                {exp with exp_desc = Texp_constant (Const_int p.Lexing.pos_lnum)};
                Lazy.force label_pos_bol,
                {exp with exp_desc = Texp_constant (Const_int p.Lexing.pos_bol)};
                Lazy.force label_pos_cnum,
                {exp with exp_desc = Texp_constant (Const_int p.Lexing.pos_cnum)}
              ],
               None)}

let quote_location exp =
  let make_absolute file =
    if Filename.is_relative file
    then Filename.concat (Sys.getcwd()) file
    else file in
  let _  = if String.length !Location.input_name = 0
  then ""
  else make_absolute !Location.input_name 
  in {exp with exp_desc =
      Texp_record([Lazy.force label_loc_start,
                   quote_position exp exp.exp_loc.Location.loc_start;
                   Lazy.force label_loc_end,
                   quote_position exp exp.exp_loc.Location.loc_end;
                   Lazy.force label_loc_ghost,
                   if exp.exp_loc.Location.loc_ghost then mktrue exp else mkfalse exp;
                 ],
                  None)}

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
  if n = 0 then begin               (*  level 0  *)
    match exp.exp_desc with
      Texp_ident _ -> exp
    | Texp_constant _ -> exp
    | Texp_let (f,pel,e) ->
        { exp with exp_desc =
          Texp_let (f,
                    List.map (map_pi2 (trx_e n)) pel, 
                    trx_e n e) }
    | Texp_function (pel, partial) ->
        { exp with exp_desc =
          Texp_function (List.map (map_pi2 (trx_e n)) pel, partial) }
    | Texp_apply (e,eool) ->
        { exp with exp_desc =
          Texp_apply (trx_e n e,
                      List.map (map_pi1 (map_option (trx_e n))) eool) }
    | Texp_match (e,pel,partial) ->
        { exp with exp_desc =
          Texp_match(trx_e n e,
                     List.map (map_pi2 (trx_e n)) pel,
                     partial) }
    | Texp_try (e,pel) ->
        { exp with exp_desc =
          Texp_try(trx_e n e,
                   List.map (map_pi2 (trx_e n)) pel) }
    | Texp_tuple el ->
        { exp with exp_desc =
          Texp_tuple (List.map (trx_e n) el)}
    | Texp_construct (cd,el) ->
        { exp with exp_desc = 
          Texp_construct (cd, List.map (trx_e n) el)}
    | Texp_variant (label, eo) ->
        { exp with exp_desc =
          Texp_variant (label, map_option (trx_e n) eo) }
    | Texp_record (del, eo) ->
        { exp with exp_desc =
          Texp_record (List.map (map_pi2 (trx_e n)) del,
                       map_option (trx_e n) eo) }
    | Texp_field (e,ld) ->
        { exp with exp_desc =
          Texp_field(trx_e n e, ld)}
    | Texp_setfield (e1,ld,e2) ->
        { exp with exp_desc =
          Texp_setfield(trx_e n e1, ld, trx_e n e2) }
    | Texp_array el ->
        { exp with exp_desc =
          Texp_array (List.map (trx_e n) el) }
    | Texp_ifthenelse (e1,e2,eo) ->
        { exp with exp_desc =
          Texp_ifthenelse (trx_e n e1,
                           trx_e n e2,
                           map_option (trx_e n) eo) }
    | Texp_sequence (e1,e2) ->
        { exp with exp_desc =
          Texp_sequence(trx_e n e1, trx_e n e2) }
    | Texp_while (e1,e2) ->
        { exp with exp_desc =
          Texp_while(trx_e n e1, trx_e n e2) }
    | Texp_for (id,e1,e2,df,e3) ->
        { exp with exp_desc = 
          Texp_for (id,
                    trx_e n e1,
                    trx_e n e2,
                    df,
                    trx_e n e3) }
    | Texp_when (e1,e2) ->
        { exp with exp_desc =
          Texp_when(trx_e n e1, trx_e n e2) }
    | Texp_send (e,m) ->
        { exp with exp_desc =
          Texp_send (trx_e n e, m)
        }
    | Texp_new (p,d) -> exp
    | Texp_instvar (p1,p2) -> exp
    | Texp_setinstvar (p1,p2,e) ->
        { exp with exp_desc =
          Texp_setinstvar(p1,p2, trx_e n e)
        }
    | Texp_override (p,pel) ->
        { exp with exp_desc =
          Texp_override(p, List.map (fun (p,e) -> (p, trx_e n e)) pel)
        }
    | Texp_letmodule (i,me,e) ->
        { exp with exp_desc =
          Texp_letmodule(i, trx_me me, trx_e n e) }
    | Texp_assert e ->
        {exp with exp_desc = Texp_assert (trx_e n e)}
    | Texp_assertfalse -> exp
    | Texp_lazy e ->
        {exp with exp_desc = Texp_lazy (trx_e n e)}
    | Texp_object ({ cl_field = cfl; cl_meths = ms },csig,sl) ->
        let cs = { cl_field = List.map trx_cf cfl; cl_meths = ms }
        in {exp with exp_desc =  Texp_object (cs,csig,sl)}
    | Texp_bracket e -> 
        let e' = trx_e (n+1) e
        in {exp with exp_desc = e'.exp_desc} 
    | Texp_escape e -> assert false
    | Texp_run e ->
        let exec = 
	  run_expression in
        {exp with
         exp_desc = Texp_apply(exec exp,
                               [(Some (trx_e n e), Required)])}
    | Texp_cspval (v,li) -> exp		(* code generator will deal with that *)
(*  | _ -> fatal_error ("Trx.trx_e level 0: case not implemented yet") *)

  end else begin                           (* level n+1 *)
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
        if stage = [] then
            let _ = Env.make_env_pure exp.exp_env in
            let _ = Env.update_ident_timestamp exp.exp_env in
            let v = (Some {exp with exp_type = instance vd.val_type})
            in call_trx_mkcsp exp v (path_to_lid i)
        else
            mkParseTree exp   (* construct VAR x *)
              (Texp_construct(Lazy.force constr_pexp_ident, 
                 [{exp with exp_type = Lazy.force type_longident_t}]))
          
    | Texp_constant cst ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_constant, 
                               [quote_constant exp cst]))

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
    | Texp_instvar (p1,p2) ->
        (* instance variables are always bound at level 0 (for now)
           so this is like a csp variable *)
        call_trx_mkcsp exp None (path_to_lid p2)
    | Texp_setinstvar _ -> fatal_error ("Trx.trx_e: setinstvar should be ruled out during type checking")
    | Texp_override  _ -> fatal_error ("Trx.trx_e: override should be ruled out during type checking")
    | Texp_letmodule (id,me,e) ->
        fatal_error ("Trx.trx_e: let module inside .<...>. not implemented yet")
          (* similar to Texp_for *)
    | Texp_assert e ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_assert, 
                               [trx_e n e]))
    | Texp_assertfalse ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_assertfalse, 
                               []))
    | Texp_lazy e ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_lazy, 
                               [trx_e n e]))

    | Texp_object (id,me,e) ->
        fatal_error ("Trx.trx_e: object ... inside .<...>. not implemented yet")

    | Texp_bracket e ->
        mkParseTree exp
          (Texp_construct(Lazy.force constr_pexp_bracket, 
                               [trx_e (n+1) e]))

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
    | Texp_cspval (m,li) ->
        call_trx_mkcsp exp None li
  end



and trx_ce ce = match ce.cl_desc with
| Tclass_ident p -> ce
| Tclass_structure { cl_field = cfl; cl_meths = ms } ->
    {ce with cl_desc =
     Tclass_structure { cl_field = List.map trx_cf cfl;
                        cl_meths = ms }
   }
| Tclass_fun (p,iel,ce,pt) ->
    {ce with cl_desc =
     Tclass_fun (p,iel, trx_ce ce, pt)
   }
| Tclass_apply (ce,eool) ->
    {ce with cl_desc =
     Tclass_apply (trx_ce ce,
                   List.map (fun (eo,o) -> (map_option (fun e -> trx_e 0 e) eo, o)) eool)
   }
| Tclass_let (rf,pel,iel,ce) ->
    let pel' = List.map (fun (p,e) -> (p, trx_e 0 e)) pel in
    let iel' = List.map (fun (i,e) -> (i, trx_e 0 e)) iel in
    let ce' = trx_ce ce in
    {ce with cl_desc = 
     Tclass_let (rf,pel',iel',ce')
   }
| Tclass_constraint (ce,sl1,sl2,c) ->
    {ce with cl_desc = 
     Tclass_constraint (trx_ce ce, sl1, sl2, c)
   }

and trx_cf = function
  | Cf_inher (ce,sil1,sil2) -> Cf_inher(trx_ce ce, sil1, sil2)
  | Cf_val (s,i,None,o) -> Cf_val(s,i,None,o)
  | Cf_val (s,i,Some e,o) -> Cf_val(s,i, Some (trx_e 0 e),o)
  | Cf_meth (s,e) -> Cf_meth(s, trx_e 0 e)
  | Cf_let (rf,pel,iel) ->
      let pel' = List.map (fun (p,e) -> (p, trx_e 0 e)) pel in
      let iel' = List.map (fun (i,e) -> (i, trx_e 0 e)) iel
      in Cf_let(rf, pel', iel')
  | Cf_init e -> Cf_init (trx_e 0 e)
        
*)

(* Functions to help the traversal and mapping of a tree.
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

let replace_pair : ('a -> 'a) -> ('b -> 'b) -> 'a *'b -> 'a * 'b =
  fun f g (x,y) ->
  match ((try Some (f x) with Not_modified -> None),
         (try Some (g y) with Not_modified -> None)) with
  | (None,None)      -> raise Not_modified
  | (Some x, None)   -> (x,y)
  | (None, Some y)   -> (x,y)
  | (Some x, Some y) -> (x,y)

(* The main function to scan the typed tree at the 0 level and
   detect brackets 
*)

let rec trx_structure str =
  {str with str_items = 
  replace_list (fun si -> {si with str_desc = trx_structure_item si.str_desc})
           str.str_items}

and trx_structure_item = function
| Tstr_eval e -> Tstr_eval (trx_expression e)
| Tstr_value (rf,pel) ->
    Tstr_value(rf, replace_list (fun (p,e) -> (p, trx_expression e)) pel)
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
    Tstr_class (replace_list (fun (dcl,sl,vf) -> (trx_ce dcl,sl,vf)) l)
| Tstr_class_type _ -> raise Not_modified
| Tstr_include (me,il) -> Tstr_include (trx_me me, il)

and trx_me me = 
  {me with mod_desc = trx_me_desc me.mod_desc} 

and trx_me_desc = function
| Tmod_ident _ -> raise Not_modified
| Tmod_structure str -> Tmod_structure (trx_structure str)
| Tmod_functor (i,l,t,me) -> Tmod_functor (i,l,t, trx_me me)
| Tmod_apply (me1,me2,mc) ->
  let (me1,me2) = replace_pair trx_me trx_me (me1,me2) in
  Tmod_apply (me1, me2, mc)
| Tmod_constraint (me,mt,mtc,mc) -> Tmod_constraint (trx_me me, mt, mtc, mc)
| Tmod_unpack (e,mt) -> Tmod_unpack (trx_expression e,mt)

and trx_expression x = failwith "na"
and trx_ce x = failwith "na"

(* Override the recursive function: public interface *)
let trx_structure str = 
  try trx_structure str with Not_modified -> str

  
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
