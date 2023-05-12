(* Report by Jeremy Yallop
   Date: Fri, 12 Jun 2015 15:32:41 +0100

While writing some code I came across the situation where a
cross-stage-persisted value is represented as a string but actually
has some other type.  Since MetaOCaml currently persists string values
directly as constants this results in a runtime exception.  Here's an
illustration:
*)
type t
val of_string : string -> t
val to_string : t -> string

(* Interestingly the error occurs only when implementation test_csp1 is in a 
separate file rather than an internal module.
*)
