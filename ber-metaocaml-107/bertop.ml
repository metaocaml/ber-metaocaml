(* `Plugin' for the OCaml top-level *)
open Longident

(* Install printers for code values *)
let install_printers () =
  Topdirs.dir_install_printer Format.std_formatter
    (Ldot(Lident "Codelib", "print_code"));
  Topdirs.dir_install_printer Format.std_formatter
    (Ldot(Lident "Codelib", "print_closed_code"))


(* Initialization function, run when toplevel starts *)

let initialize () =
  Printf.printf "BER MetaOCaml toplevel, version %s\n" Trx.meta_version;
  install_printers ()

(* Hook up to the top level, run BEFORE the toplevel starts *)
(* The task here is to set up the hooks plus adjust the flags.
   We set Clflags.real_paths to false (which corresponds to the flag
   -short-paths) so to print the type Trx.code as mere code.
   We also arrange to open Codelib.
*)
let () =
 Clflags.real_paths := false;
 Clflags.open_modules := "Codelib" :: !Clflags.open_modules;
 Toploop.toplevel_startup_hook :=
  let old_hook = !Toploop.toplevel_startup_hook in
  fun () -> 
    begin
      initialize ();
      old_hook ()
    end

