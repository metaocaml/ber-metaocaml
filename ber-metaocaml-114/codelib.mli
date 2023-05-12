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

(* What to do about hardly serializable CSPs *)
type csp_handling = Trx.csp_handling = CSP_ok | CSP_warn | CSP_error

(* Check that the code is closed and return the closed code *)
val close_code : ?csp:csp_handling -> 'a code -> 'a closed_code

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code:
   If there is a scope extrusion error, it is still useful
   to show the code with the extrusion before throwing the scope-extrusion
   exception.
*)
val close_code_delay_check : ?csp:csp_handling ->
  'a code -> 'a closed_code * (unit -> unit)

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

(* Utility functions for code construction *)
(* sequencing *)
val seq  : unit code -> 'a code -> 'a code
val seqs : unit code list -> unit code (* folding over unit code monoid *)

(* make a match statement *)
val make_match : 'a code -> ('a -> 'w) pat_code list -> 'w code

(* let-insertion *)
type locus = Trx.locus
val locus_global : locus

val genletv : ?name:string -> ?locus:locus -> 'a code -> 'a val_code

(* The result of genletv coerced to 'a code so that it can
   be immediately used in a splice. This is a very common pattern.
*)
val genlet : ?name:string -> ?locus:locus -> 'a code -> 'a code

val with_locus : (locus -> 'w code) -> 'w code

(* Local let-insertion: generate the let-expression right-away
  However, if the expression to bind is simple (is a value whose construction
  does not cause big memory allocations), no let is generated.
  One may say that the generated let is immediately beta-reduced away.
  The optional ?name is the hint about the generated name
 *)
val letlv : ?name:string -> 'a code -> (('a val_code -> 'w code) -> 'w code)

(* The result of letlv coerced to 'a code so that it can
   be immediately used in a splice. This is a very common pattern.
*)
val letl : ?name:string -> 'a code -> (('a code -> 'w code) -> 'w code)

(* Potentially mutually recursive memoizing let-insertion *)
type locus_rec

val mkgenlet : ?name:string ->          (* hint of names to bind *)
  locus_rec ->                          (* locus created by with_locus_rec *)
  ('key->'key->bool) ->                 (* memo key comparison *)
  (* The result: genletrec, which takes a key and 
       a function to produce the expression to let-bind
  *)
  (('key -> ('a->'b) code) -> 'key -> ('a->'b) code)


(* Create a locus_rec and generate let rec statement at that locus *)
val with_locus_rec : (locus_rec -> 'w code) -> 'w code


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
