(* Printing code expressions *)

open Runcode

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
