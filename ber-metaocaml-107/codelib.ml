(* Code Prelude
   The initially open module in MetaOCaml

   It describes the code handling facilities other than brackets and escapes.
   (Running the code is in separate runcode and runnative modules)

   This present module applies to both native and bytecode modes 
*)

open Format

type 'a closed_code = Trx.closed_code_repr

type 'a code     = 'a Trx.code
type 'a pat_code = 'a Trx.pat_code
type 'a val_code = 'a Trx.val_code

(* Check that the code is closed and return the closed code *)
let close_code : 'a code -> 'a closed_code = fun cde ->
  Trx.close_code_repr (cde :'a code :> Trx.code_repr)

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code.
*)
let close_code_delay_check : 'a code -> 'a closed_code * (unit -> unit) =
  fun cde -> Trx.close_code_delay_check (cde : 'a code :> Trx.code_repr)

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

(* `Format' close code value as an AST *)
let ast_of_code : 'a closed_code -> Parsetree.expression = fun cde ->
 (cde : Trx.closed_code_repr :> Parsetree.expression)


(* First-class pattern-match. See Trx.make_match for details.
*)
let make_match : 'a code -> ('a -> 'w) pat_code list -> 'w code =
  fun[@warning "-8"] scrutinee cases ->
    let scrutinee = (scrutinee : 'a code :> Trx.code_repr) in
    let cases = 
      List.map (fun x -> (x : ('a -> 'w) pat_code :> Trx.code_repr))
       cases in
    Obj.magic (Trx.make_match scrutinee cases)


(* let-insertion *)
let genletv : 'a code -> 'a val_code = fun cde -> 
  let cde = (cde : 'a code :> Trx.code_repr) in
  Obj.magic @@ Trx.genlet cde

(* The result of genletv coerced to 'a code so that it can
   be immediately used in a splice. This is a very common pattern.
*)
let genlet : 'a code -> 'a code = fun cde -> 
  (genletv cde : 'a val_code :> 'a code)

(* Liftable types
   The types of present-stage values that are OK to use at a future
   stage (values that are safe to CSP, both in byte- and native-code modes)
*)

module type lift = sig
  type t
  val lift: t -> t code
end
