(**			Generic print

 The interface is similar to that in toplevel/printval.mli

$Id: gprint.mli,v 1.1 2006/04/15 11:27:37 oleg Exp $
*)


(* The output is the type of the printed expression, as a string *)
val fprint : Format.formatter -> ('a,'b) code -> string 
val print  : ('a,'b) code -> string

val max_printer_depth: int ref
val max_printer_steps: int ref
