(* `Plugin' for the OCaml top-level *)
open Longident

(* Install printers for code values *)
let install_printers () =
  Topdirs.dir_install_printer Format.std_formatter
    (Ldot(Lident "Print_code", "print_code"));
  Topdirs.dir_install_printer Format.std_formatter
    (Ldot(Lident "Print_code", "print_cde"))

(* Initialization function *)

let initialize () =
  Printf.printf "BER MetaOCaml toplevel, version %s\n" Trx.meta_version;
  install_printers ()

(* Hook up to the top level *)
let () =
Toploop.toplevel_startup_hook :=
  let old_hook = !Toploop.toplevel_startup_hook in
  fun () -> 
    begin
      initialize ();
      old_hook ()
    end

