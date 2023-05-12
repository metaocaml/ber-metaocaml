(* Run the closed code: byte-code 
   We now rely as much as possible on the Toploop-provided public
   interfaces, without any internals (like load_lambda, etc)
   Unfortutanely, execute_phrase only prints the result of the
   evaluation, but does not let us get hold of the raw result.
   Therefore, given the source code expression exp, we execute
   let result__ = Obj.repr exp
   and then retrieve the value of result__ from the top-level
   env (resetting it to Obj.repr 0 afterwords to stem memory leaks)
 *)

open Format
open Codelib

(* Add a directory to search for .cmo/.cmi files, needed
   for the sake of running the generated code .
   The specified directory is prepended to the load_path.
*)
let add_search_path : string -> unit = fun dir ->
  let dir = Misc.expand_directory Config.standard_library dir in
  Load_path.add_dir dir;
  Dll.add_path [dir];
  Env.reset_cache ()


(* Execute a thunk (which does compilation) while disabling certain
   warnings.
   Warning 20 often comes from let's remaining from CSPs
*)
let warnings_descr = "-8-20-26-27"

let with_disabled_warnings warnings thunk =
  let warnings_old = Warnings.backup () in
  let _ = Warnings.parse_options false warnings in
  try
    let r = thunk () in
    Warnings.restore warnings_old; r
  with e ->
    Warnings.restore warnings_old;
    raise e

(*
    try initialize_toplevel_env ()
      Warnings.reset_fatal ();
      Env.reset_cache_toplevel ();
      ignore(execute_phrase false ppf phr)
*)

let initial_env = ref Env.empty

(* See toploop/toploop.ml: initialize_toplevel_env
   That function calles Compmisc.initial_env
   What we do is to do most of its job without calling Ident.reinit(),
   which corrupts the timestamps and interferes with toploop

let initialize_toplevel_env () =
  (*
  toplevel_env := Compmisc.initial_env()
  *)
  (* Ident.reinit(); *)
  (* Types.Uid.reinit(); *)
  Typemod.initial_env
    ~loc:(Location.in_file "metaocaml file")
    ~safe_string:true
    ~initially_opened_module:(Some "Stdlib")
    ~open_implicit_modules:(List.rev !Clflags.open_modules)
*)

(* Patterned after toploop.ml:execute_phrase *)
let typecheck_code' : Parsetree.expression -> Typedtree.structure = fun exp ->
  if !initial_env = Env.empty then
    initial_env := !Toploop.toplevel_env;
  let ppf = std_formatter in
  with_disabled_warnings warnings_descr
 (fun () ->
   let sstr = [Ast_helper.Str.eval exp] in
   if !Clflags.dump_source then Pprintast.structure ppf sstr;
   try
     Typecore.reset_delayed_checks ();
     let (str, _sg, _sn, _shape, _newenv) =
        Typemod.type_toplevel_phrase !initial_env sstr in
     Typecore.force_delayed_checks (); 
     Warnings.check_fatal ();
     str
   with 
    x -> (Errors.report_error ppf x;
	  Format.pp_print_newline ppf ();
	  failwith 
            "Error type-checking generated code: file a bug report")
 )


(* For the benefit of offshoring, etc. *)
let typecheck_code : 'a closed_code -> Typedtree.expression = fun cde ->
  let str = typecheck_code' @@ ast_of_code cde in
  match str.Typedtree.str_items with 
  | [{Typedtree.str_desc = Typedtree.Tstr_eval (texp,_)}] -> texp
  | _  -> failwith "cannot happen: Parsetree was not an expression?"


let result_var = "result__"

let run_bytecode : 'a closed_code -> 'a = fun cde ->
  let phrase =
    let open Ast_helper in
    let open Asttypes in
    Parsetree.Ptop_def [
    Str.value Nonrecursive 
        [Vb.mk (Pat.var (Location.mknoloc result_var)) (ast_of_code cde)]]
  in
  let fmt = Format.err_formatter in
  if with_disabled_warnings warnings_descr (fun () ->
    Toploop.execute_phrase false fmt phrase) 
  then
    let res = Toploop.getvalue result_var in
    Toploop.setvalue result_var (Obj.repr 0);
    Obj.obj res
  else
   failwith "Code execution failed"

(* Abbreviations for backwards compatibility *)
let run cde = run_bytecode (close_code ~csp:CSP_ok cde)
let (!.) cde = run cde

