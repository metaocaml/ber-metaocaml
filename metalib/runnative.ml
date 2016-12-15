(* Given a closed code expression, compile it with the *native*
   compiler, linked it in and run returning
   its result or propagating raised exceptions.
*)

open Print_code
open Format

let load_path : string list ref = ref []

(* Add a directory to search for .cmo/.cmi files, needed
   for the sake of running the generated code .
   The specified directory is prepended to the load_path.
*)
let add_search_path : string -> unit = fun dir ->
  load_path := dir :: !load_path

(*
 Dynlink library can only load the unit and evaluate its top-level
 expressions. There is no provision for accessing the names defined 
 in the loaded unit. Therefore, the only way to get the result is
 to assign it to an reference cell defined in the main program.
 This file defines "result__" exactly for this purpose.

 Given the code cde, we generate a file

 Runnative.result__ := Some (Obj.repr (cde))

*)

(* The reference cells contains something other than None for a brief
   period, before the value is taken and returned to the caller of
   runnative. This policy prevents memory leaks
*)
let result__ : Obj.t option ref = ref None

let code_file_pref = "runn"

let run_native : 'a closed_code -> 'a = fun cde ->
  if not Dynlink.is_native then
    failwith "run_native only works in the native code";
  let (fname,oc) =
    Filename.open_temp_file ~mode:[Open_wronly;Open_creat;Open_text]
      code_file_pref ".ml" in
  let ppf = formatter_of_out_channel oc in
  let ()  = fprintf ppf
      "Runnative.result__ := Some (Obj.repr (%a))@."
      format_code cde in
  let () = close_out oc in
  let () = eprintf "Created the code file %s\n" fname in
  let () = Dynlink.loadfile_private fname in
  match !result__ with
  | None -> assert false                (* can't happen *)
  | Some x -> 
      result__ := None;                 (* prevent the memory leak *)
      Obj.obj x


(* Abbreviations for backwards compatibility *)
let run cde = run_native (close_code cde)
let (!.) cde = run cde
