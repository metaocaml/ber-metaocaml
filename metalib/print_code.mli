(* Printing code expressions. Works both in native and bytecode modes *)

(* Common interface *)

type 'a closed_code = Trx.closed_code_repr

(* Check that the code is closed and return the closed code *)
val close_code : 'a code -> 'a closed_code

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code:
   If there is a scope extrusion error, it is still useful
   to show the code with the extrusion before throwing the scope-extrusion
   exception.
*)
val close_code_delay_check : 'a code -> 'a closed_code * (unit -> unit)

(* Total: a closed code can always be used in slices, etc. *)
val open_code : 'a closed_code -> 'a code

(* Total: 'a pat_code is a `subtype' of 'a code *)
val code_of_pat_code : 'a Trx.pat_code -> 'a code

(* Total: 'a val_code is a `subtype' of 'a code *)
val code_of_val_code : 'a Trx.val_code -> 'a code

(* Print code values, useful as formatter.
   The code is printed with outer brackets 
*)
val print_code         : Format.formatter -> 'a code -> unit
val print_closed_code  : Format.formatter -> 'a closed_code -> unit

(* Like print_closed_code, but omit the outer brackets.
   This function is useful when saving the generated code into a file,
   to compile later.
*)
val format_code : Format.formatter -> 'a closed_code -> unit

(* print code as a parse tree. Useful for debugging *)
val print_code_as_ast : 'a closed_code -> unit

(* make a match statement *)
val make_match : 'a code -> ('a -> 'w) Trx.pat_code list -> 'w code
