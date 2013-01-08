(* Printing code expressions *)


(* low-level pretty-printers of the Ast *)
val inpc              : Format.formatter -> Parsetree.expression -> unit
val top_phrase_pretty : Format.formatter -> Parsetree.toplevel_phrase -> unit
val inpc_string       : Parsetree.expression -> string

(* Print code values *)
val print_code : Format.formatter -> ('c,'a) code -> unit
val print_cde  : Format.formatter -> 'a Runcode.cde -> unit

(* print code as a parse tree. Useful for debugging *)
val print_code_as_ast : ('a, 'b) code -> unit
