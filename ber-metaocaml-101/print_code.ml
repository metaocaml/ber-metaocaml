(* Printing code expressions *)

(* The original code was authored by  Ed Pizzi
   and simplified by Jacques Carette.
   It is latter borrowed into the main OCaml tree,
   as parsing/pprintast.ml.
   It was extensively rewritten by Hongbo Zhang: University of Pennsylvania
   and modified by Thomas Gazagnaire (OCamlPro) and
   Fabrice Le Fessant (INRIA Saclay).

   We now rely on the OCaml's code.
*)

open Format
open Runcode
open Parsetree


(* print code as a parse tree. Useful for debugging *)
let print_code_as_ast cde =
  let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
  Printast.implementation Format.std_formatter
  [{ pstr_desc = Pstr_eval (cde);
     pstr_loc  = Location.none }]

let format_code : Format.formatter -> 'a closed_code -> unit = fun ppf cde ->
  let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
  Pprintast.expression ppf cde

(* These functions are suitable for installing as printers
   at the toplevel, using top-level directive install printer.
   Don't rename these functions or change their types.
   See bertop.ml, which refers to these functions by their external
   symbolic name.
*)

let print_closed_code  : Format.formatter -> 'a closed_code -> unit = 
  fun ppf cde ->  
    Format.fprintf ppf ".<@,%a>.@ " format_code cde

let print_code ppf (cde : 'a code) = 
  let (cde, check) = close_code_delay_check cde in
  print_closed_code ppf cde;
  try check ()
  with e -> fprintf ppf "\n%s" (Printexc.to_string e)
