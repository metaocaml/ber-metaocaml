(* Run the closed code: byte-code and native code *)

open Format
open Parsetree

type 'a cde = {cde : 'c. ('c,'a) code}  (* Type of the closed code *)

(* Cast the closed code to Typedtree.expression, 
   revealing its representation
*)
let cde_repr {cde = x} = Obj.magic x


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
  let curr_str = 
    String.concat "" 
      (List.map 
	 (fun w -> 
	   let state = Warnings.is_active w in
	   (if state then fst else snd) (List.assoc w warnings_descr))
	 warnings) in
  let () = Warnings.parse_options false disable_str in
  try
    let r = thunk () in
    Warnings.parse_options false curr_str; r
  with e ->
    Warnings.parse_options false curr_str;
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
  try
    let retval = (Meta.reify_bytecode code code_size) () in
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    retval
  with x ->
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Symtable.restore_state initial_symtable;
    raise x


let run' exp =
  let exp = Trx.check_scope_extrusion exp in
  if !initial_env = Env.empty then initial_env := Compile.initial_env();
  Ctype.init_def(Ident.current_time()); 
  with_disabled_warnings [Warnings.Partial_match "";
			  Warnings.Unused_argument;
			  Warnings.Unused_var "";
			  Warnings.Unused_var_strict ""]
 (fun () ->
   let sstr = [{pstr_desc = Pstr_eval exp; pstr_loc = Location.none}] in
   let str = try
    begin
       Typecore.reset_delayed_checks ();
       let (str, sg, newenv) = Typemod.type_toplevel_phrase !initial_env sstr in
       Typecore.force_delayed_checks (); str
    end
   with 
    x -> (Errors.report_error Format.std_formatter x;
	  Format.pp_print_newline Format.std_formatter ();
	  failwith 
            "Error type-checking generated code: scope extrusion?")
  in
  let lam = Translmod.transl_toplevel_definition str in
  load_lambda Format.std_formatter lam
 )

let run exp =
  let exp = cde_repr exp in		(* reveal concerete representation *)
  Obj.obj (run' exp)
