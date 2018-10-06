(* Code Prelude
   The initially open module in MetaOCaml

   It describes the code handling facilities other than brackets and escapes.
   (Running the code is in separate runcode and runnative modules)

   This present module applies to both native and bytecode modes 
*)

type 'a closed_code
type 'a code     = 'a Trx.code
type 'a pat_code = 'a Trx.pat_code
type 'a val_code = 'a Trx.val_code

(* 'a pat_code and 'a val_code are `subtypes' of 'a code 
   Use coercion to turn 'a pat_code to 'a code:
   (x : 'a pat_code :> 'a code)
*)

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

(* `Format' close code value as an AST *)
val ast_of_code : 'a closed_code -> Parsetree.expression

(* make a match statement *)
val make_match : 'a code -> ('a -> 'w) pat_code list -> 'w code

(* let-insertion *)
val genletv : 'a code -> 'a val_code

(* The result of genletv coerced to 'a code so that it can
   be immediately used in a splice. This is a very common pattern.
*)
val genlet  : 'a code -> 'a code

(* Liftable types
   The types of present-stage values that are OK to use at a future
   stage (values that are safe to CSP, both in byte- and native-code modes)

   Although polymorphic lift is possible in MetaOCaml and works well
   in byte-code, it is not at all helpful in the native mode.
*)

module type lift = sig
  type t
  val lift: t -> t code
end
