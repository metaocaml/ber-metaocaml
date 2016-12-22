(* Printing code expressions and auxiliary functions. Work in both
  native and bytecode.
*)

open Format

(* Common interface for running the code *)

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

(* The original code was authored by  Ed Pizzi
   and simplified by Jacques Carette.
   It is latter borrowed into the main OCaml tree,
   as parsing/pprintast.ml.
   It was extensively rewritten by Hongbo Zhang: University of Pennsylvania
   and modified by Thomas Gazagnaire (OCamlPro) and
   Fabrice Le Fessant (INRIA Saclay).

   We now rely on the OCaml's code.
*)


(* print code as a parse tree. Useful for debugging *)
let print_code_as_ast cde =
  let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
  Printast.implementation Format.std_formatter
  [Ast_helper.Str.eval cde]

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

let make_match : 'a code -> ('a -> 'w) Trx.pat_code list -> 'w code =
  fun scrutinee cases ->
    let scrutinee = (close_code scrutinee : Trx.closed_code_repr :> Parsetree.expression) in
    let open Parsetree in
    let ast =
    match cases with
    | [c] -> 
        begin
          match (c : ('a->'w) Trx.pat_code :> expression).pexp_desc 
          with
          | Pexp_function caselist ->
              Ast_helper.Exp.match_ scrutinee caselist
          | _ -> assert false
        end
    | _ -> assert false
    in
    let ast_repr : Trx.closed_code_repr = Obj.magic ast in
    open_code ast_repr


        
