(* Given a closed code expression, compile it with the *native*
   compiler, link it in, and run returning
   its result or propagating raised exceptions.
*)

open Codelib
open Format

let load_path : string list ref = ref []

(* Add a directory to search for .cmo/.cmi files, needed
   for the sake of running the generated code .
   The specified directory is prepended to the load_path.
*)
let add_search_path : string -> unit = fun dir ->
  load_path := dir :: !load_path

let ocamlopt_path = 
  let open Filename in
  concat (dirname (Config.standard_runtime)) "ocamlopt"

(* Compile the source file and make the .cmxs, returning its name *)
let compile_source : string -> string = fun src_fname ->
  let basename = Filename.remove_extension src_fname in
  let plugin_fname =  basename ^ ".cmxs" in
  let other_files  =  [basename ^ ".cmi"; basename ^ ".cmx";
                       basename ^ ".o"] in
  let cmdline = ocamlopt_path ^ 
                " -shared" ^
                " -o " ^ plugin_fname ^
                (String.concat "" @@ 
                 List.map (fun p -> " -I " ^ p) !load_path) ^
                " " ^ src_fname in         
  let rc = Sys.command cmdline in
  List.iter Sys.remove other_files;
  if rc = 0 then plugin_fname else 
    let () = Sys.remove plugin_fname in
    failwith "runnative: .cmxs compilation failure"

(*
 Dynlink library can only load the unit and evaluate its top-level
 expressions. There is no provision for accessing the names defined 
 in the loaded unit. Therefore, the only way to get the result is
 to assign it to a reference cell defined in the main program.
 This file defines "result__" exactly for this purpose.

 Given the code cde, we generate a file

 Runnative.result__ := Some (Obj.repr (cde))

which we then compile and link in.
*)

(* The reference cell below contains something other than None for a brief
   period, before the value is taken and returned to the caller of
   runnative. This policy prevents memory leaks
*)
let result__ : Obj.t option ref = ref None

let code_file_prefix = "runn"

(* Create a file to compile and later link, using the given closed code *)
let create_comp_unit : 'a closed_code -> string = fun cde ->
  let (fname,oc) =
    Filename.open_temp_file ~mode:[Open_wronly;Open_creat;Open_text]
      code_file_prefix ".ml" in
  let ppf = formatter_of_out_channel oc in
  let ()  = fprintf ppf
      "Runnative.result__ := Some (Obj.repr (%a))@."
      format_code cde in
  let () = close_out oc in
  fname                                 (* let the errors propagate *)


let run_native : 'a closed_code -> 'a = fun cde ->
  if not Dynlink.is_native then
    failwith "run_native only works in the native code";
  let source_fname = create_comp_unit cde in
  let plugin_fname = compile_source source_fname in
  let () = Dynlink.loadfile_private plugin_fname in
  Sys.remove plugin_fname;
  Sys.remove source_fname;
  match !result__ with
  | None -> assert false                (* can't happen *)
  | Some x -> 
      result__ := None;                 (* prevent the memory leak *)
      Obj.obj x
  (* If an exception is raised, leave the source and plug-in files,
     so to investigate the problem.
   *)

(* Abbreviations for backwards compatibility *)
let run cde = run_native (close_code cde)
let (!.) cde = run cde
