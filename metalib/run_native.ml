(* Support for running natively compiled code *)

(*
Here we collect various fragments, to be work on later.

The first fragment used to be in driver/optcompile.ml. It is definitely
a hack! Rather than patch every implementation file with extra code to
restore the array of CSP values, we should create a (perhaps C)
code file that maintains the CSP array, storing and restoring it as needed,
probably as part of initialization.
That code file will be linked in with the whole project (forcibly linked,
via the -linkall option).
The Trx module will refer to the name of the CSP array, when processing
CSPs in the native code.
*)

open Parsetree
let local_csp_arr_name = "local_csp_arr_hwrrekjhj"
let local_csp_arr_str () =
  let exp = {pexp_desc = Obj.magic ();
             pexp_loc = Location.none;
             pexp_ext = None }
  and pat = {ppat_desc = Obj.magic ();
             ppat_loc = Location.none;
             ppat_ext = None } in
  let exp1 = {exp with pexp_desc = Pexp_ident (Longident.Ldot (Longident.Lident "Marshal", "from_string"))}
  and exp2 = {exp with pexp_desc = Pexp_constant (Asttypes.Const_string "blaaaaah!")}
  and exp0 = {exp with pexp_desc = Pexp_constant (Asttypes.Const_int 0)} in
  let exp3 = {exp with pexp_desc = Pexp_apply ( exp1, [("",exp2);("",exp0)])} in
  let pat1 = {pat with ppat_desc = Ppat_var local_csp_arr_name} in
  let str1 = {pstr_desc = Pstr_value (Asttypes.Nonrecursive,[pat1,exp3]);
              pstr_loc = Location.none} in
  let pat2 = {pat with ppat_desc = Ppat_any} in
  let exp4 = {exp with pexp_desc = Pexp_ident (Longident.Lident local_csp_arr_name)} in
  let str2 = {pstr_desc = Pstr_value (Asttypes.Nonrecursive,[pat2,exp4]);
              pstr_loc = Location.none} in
  let _ = Trx.reset_csp_array ()
  in [str1;str2]

let find_local_csp_arr_texp =
  function | Tstr_value (_,[p,e]) -> e
           | _ -> assert false

let hack_csp_arr_str = function
  | Tstr_value (rf,[p,e]) ->
    Tstr_value (rf,[p,{e with exp_desc =
      begin
      match e.exp_desc with
      | Texp_apply (e,[(Some e1,o);a]) ->
        Texp_apply (e,[(Some {e1 with exp_desc =
          begin
	  match e1.exp_desc with
	  | Texp_constant (Asttypes.Const_string "blaaaaah!") ->
	      let s = Marshal.to_string !Trx.csp_array []
	      in Texp_constant (Asttypes.Const_string s)	      
	  | _ -> assert false
          end}, o);a])
      | _ -> assert false
      end}])
  | _ -> assert false

let process_str str =
  Trx.initial_native_compilation := true;
  if !Clflags.plain_ocaml then str
  else let str2 = List.hd(List.tl str)
       in Trx.local_csp_arr_texp := (find_local_csp_arr_texp str2);
       let str' = Trx.trx_structure str
       in (hack_csp_arr_str (List.hd str'))::(List.tl str')

(*The  local_csp_arr_str was hooked up in optcompile.ml before
   Typemod.type_implementation:
      ++ (fun str ->(if !Clflags.plain_ocaml then str else (local_csp_arr_str ()@str))) (* XXO *)
and process_str was hooked up after Typemod.type_implementation
*)


let plugin_count = ref 0
let execute_expression_native exp =
  init_path ();
  begin (* Update the global ident timestamp *)
    match exp.pexp_ext with
    | Some v -> let t = Env.get_ident_timestamp (Obj.magic v).exp_env
                in Ident.set_current_time t
    | None -> ()
  end;
  Ctype.init_def(Ident.current_time());
  let exp1 = { exp with pexp_desc = Pexp_ident (Longident.Ldot
     (Longident.Lident "Trx", "execute_expression_result")) } in
  let exp2 = { exp with pexp_desc = Pexp_ident (Longident.Lident ":=") } in
  let exp3 = { exp with pexp_desc = Pexp_ident (Longident.Ldot
     (Longident.Lident "Obj", "magic")) } in
  let exp4 = { exp with pexp_desc = Pexp_apply ( exp3, [("",exp)]) } in
  let exp5 = { exp with pexp_desc = Pexp_apply ( exp2,
        [("",exp1);("",exp4)]) } in
  let ppf = Format.std_formatter in
  let sourcefile = begin
	             incr plugin_count;
		     "plugin" ^ (string_of_int !plugin_count) ^ ".ml"
		   end in
  let prefixname = Misc.chop_extension_if_any sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = Pparse.preprocess sourcefile in
  let env = initial_env() in
  Compilenv.reset modulename;
  (* Clflags.keep_asm_file := true; *)
  try
    [{ Parsetree.pstr_desc = Parsetree.Pstr_eval exp5;
       Parsetree.pstr_loc = Location.none }]
    ++ Typemod.type_implementation sourcefile prefixname modulename env
      (* The following should not be needed: we added Trx pre-processing
	 already in the type checker.
      *)
    ++ (fun (str,coe) -> (Trx.trx_structure str, coe)) (* XXO *)
    ++ Translmod.transl_store_implementation modulename
    +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
    +++ Simplif.simplify_lambda
    +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
    ++ Asmgen.compile_implementation prefixname ppf;
    Compilenv.save_unit_info (prefixname ^ ".cmx");
    Warnings.check_fatal ();
    Pparse.remove_preprocessed inputfile;
    !Trx.load_compiled_code_hook !plugin_count;
    !Trx.execute_expression_result
  with x ->
    Pparse.remove_preprocessed_if_ast inputfile;
    raise x

let _ = Trx.native_mode := true
let _ = Trx.execute_expression_hook := execute_expression_native


(* The following code is moved from trx.ml *)
let pathval_trx_get_csp_value = lazy(find_value "Trx.get_csp_value")
let pathval_array_get = lazy(find_value "Array.get")
let trx_array_get exp =
  let (p, v) = Lazy.force pathval_array_get in
  { exp with exp_type = instance v.val_type;
    exp_desc = Texp_ident(p, v) }

let trx_get_csp_value exp =
  let (p, v) = Lazy.force pathval_trx_get_csp_value in
  { exp with exp_type = instance v.val_type;
    exp_desc = Texp_ident(p, v) }

let initial_native_compilation = ref false
let execute_expression_hook = ref (fun _ -> assert false)
let execute_expression_result = ref (Obj.repr 7)
let execute_expression e = !execute_expression_hook e
let load_compiled_code_hook = ref (fun n -> ())

let empty_csp_array = Array.create 1000 (Obj.repr ())
let csp_array = ref (Array.copy empty_csp_array)
let csp_index = ref 0
let reset_csp_array () =
  csp_array := Array.copy empty_csp_array;
  csp_index := 0
let csp_arr_filename = "saved_csp_array"

let save_csp_array () = if !csp_index <> 0 then
  let outchan = open_out csp_arr_filename in
  let v = (!csp_index, !csp_array)
  in Marshal.to_channel outchan v []; close_out outchan

let load_csp_array () =
  let inchan = open_in csp_arr_filename in
  let (len,a) = Marshal.from_channel inchan
  in csp_index := len; csp_array := a;
  close_in inchan

let add_csp_value (v,l) =
  begin
    if (!csp_index >= (Array.length !csp_array))
    then csp_array := Array.append !csp_array empty_csp_array
  end;
  !csp_array.(!csp_index) <- v;
  incr csp_index;
  !csp_index - 1

let get_csp_value  n = !csp_array.(n)


let local_csp_arr_texp = ref (Obj.magic ())


let remove_texp_cspval exp =
  if !native_mode = false then exp else
  failwith "native mode CSP are not impemented yet"
(*
 ZZZ
  match exp.exp_desc with
  | Texp_cspval (v,l) ->
      let i = add_csp_value (v,l) in
      let exp' = {exp with exp_desc = Texp_constant (Const_int i)} in
      let desc = if !initial_native_compilation
        then (Texp_apply (trx_array_get exp, [(Some !local_csp_arr_texp, Required);(Some exp', Required)]))
	else (Texp_apply (trx_get_csp_value exp, [(Some exp', Required)])) in
      {exp with exp_desc = desc}
  | _ -> assert false
*)


(*
 Moved from trx.mli
val native_mode : bool ref
val initial_native_compilation : bool ref
val get_csp_value : int -> Obj.t
val execute_expression_hook : (Parsetree.expression -> Obj.t) ref
val load_compiled_code_hook : (int -> unit) ref
val execute_expression_result : Obj.t ref
val execute_expression : Parsetree.expression -> Obj.t
val csp_array : Obj.t array ref
val reset_csp_array : unit -> unit
val local_csp_arr_texp : Typedtree.expression ref
*)
