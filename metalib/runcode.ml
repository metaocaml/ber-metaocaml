(* Run the closed code: byte-code and native code *)

open Format

type 'a closed_code = Trx.closed_code_repr


(* Check that the code is closed and return the closed code *)
let close_code : 'a code -> 'a closed_code = fun cde ->
  Trx.close_code_repr (Obj.magic cde)

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code.
*)
let close_code_delay_check : 'a code -> 'a closed_code * (unit -> unit) =
  fun cde -> Trx.close_code_delay_check (Obj.magic cde)

let open_code : 'a closed_code -> 'a code = fun ccde ->
  Obj.magic (Trx.open_code ccde)


(* Execute a thunk (which does compilation) while disabling certain
   warnings.
*)
let warnings_descr =
    [(Warnings.Partial_match "",("P","p"));
     (Warnings.Unused_argument,("X","x"));
     (Warnings.Unused_var "",("Y","y"));
     (Warnings.Unused_var_strict "",("Z","z"))
    ]

let with_disabled_warnings warnings thunk =
  let disable_str =
    String.concat "" 
      (List.map 
	 (fun w -> snd (List.assoc w warnings_descr)) warnings) in
(*
  let curr_str = 
    String.concat "" 
      (List.map 
	 (fun w -> 
	   let state = Warnings.is_active w in
	   (if state then fst else snd) (List.assoc w warnings_descr))
	 warnings) in
*)
  let warnings_old = Warnings.backup () in
  let () = Warnings.parse_options false disable_str in
  try
    let r = thunk () in
    Warnings.restore warnings_old; r
  with e ->
    Warnings.restore warnings_old;
    raise e


let initial_env = ref Env.empty

(* Load and execute bytecode: copied from toploop/toploop.ml *)
let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then
    fprintf ppf "%a%a@."
    Printinstr.instrlist init_code
    Printinstr.instrlist fun_code;
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  (* let initial_bindings = !toplevel_value_bindings in *)
  try
    Toploop.may_trace := true;
    let retval = (Meta.reify_bytecode code code_size) () in
    Toploop.may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    retval
  with x ->
    Toploop.may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    (* let initial_bindings = !toplevel_value_bindings in *)
    Symtable.restore_state initial_symtable;
    raise x

(* Patterned after toploop.ml:execute_phrase *)

let typecheck_code' : Parsetree.expression -> Typedtree.structure = fun exp ->
  if !initial_env = Env.empty then begin
    let old_time = Ident.current_time() in
    (* does Ident.reinit() and may corrupt the timestamp if we
       run in top-level. See Ident.reinit code
     *)
    initial_env := Compmisc.initial_env(); 
    Ident.set_current_time old_time
   end;
  (* Ctype.init_def(Ident.current_time());  *)
  let ppf = std_formatter in
  with_disabled_warnings [Warnings.Partial_match "";
			  Warnings.Unused_argument;
			  Warnings.Unused_var "";
			  Warnings.Unused_var_strict ""]
 (fun () ->
   let sstr = [Ast_helper.Str.eval exp] in
   if !Clflags.dump_source then Pprintast.structure ppf sstr;
   try
    begin
       Typecore.reset_delayed_checks ();
       let (str, sg, newenv) = Typemod.type_toplevel_phrase !initial_env sstr in
       if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
       let sg' = Typemod.simplify_signature sg in
       if !Clflags.dump_typedtree then Printtyp.signature ppf sg';
       ignore (Includemod.signatures !initial_env sg sg');
       Typecore.force_delayed_checks (); str
    end
   with 
    x -> (Errors.report_error ppf x;
	  Format.pp_print_newline ppf ();
	  failwith 
            "Error type-checking generated code: scope extrusion?")
 )

(* For the benefit of offshoring, etc. *)
let typecheck_code : 'a closed_code -> Typedtree.expression = fun cde ->
  let str = typecheck_code' 
      (cde : Trx.closed_code_repr :> Parsetree.expression) in
  match str.Typedtree.str_items with 
  | [{Typedtree.str_desc = Typedtree.Tstr_eval (texp,_)}] -> texp
  | _  -> failwith "cannot happen: Parsetree was not an expression?"

let run_bytecode : 'a closed_code -> 'a = fun cde ->
  let str = typecheck_code' 
      (cde : Trx.closed_code_repr :> Parsetree.expression) in
  let lam = Translmod.transl_toplevel_definition str in
  Warnings.check_fatal ();
  Obj.obj @@ load_lambda Format.std_formatter lam

(* Abbreviations for backwards compatibility *)
let run cde = run_bytecode (close_code cde)
let (!.) cde = run cde

