(* Trivial tests of MetaOCaml, which are also regression tests *)

3+2;;
(* - : int = 5 *)
let rec fact = function | 0 -> 1 | n -> n * fact (n-1);;
(* val fact : int -> int = <fun> *)
fact 5;;
(* - : int = 120 *)

.<1>.;;
(* - : ('cl, int) code = .<1>. *)
.<"aaa">.;;
(* - : ('cl, string) code = .<"aaa">. *)
.! .<1>.;;
(* - : int = 1 *)


.<fun x -> .~(let y = x in y)>.;;
(*
Characters 22-23:
  .<fun x -> .~(let y = x in y)>.;;
                        ^
Error: Wrong level: variable bound at level 1 and used at level 0
*)

.<fun x -> 1 + .~(.<true>.)>.;;
(*
Characters 20-24:
  .<fun x -> 1 + .~(.<true>.)>.;;
                      ^^^^
Error: This expression has type bool but an expression was expected of type
         int
*)

.<fun x -> .~(.! .<x>.; .<1>.)>.;;
(*
Characters 14-22:
  .<fun x -> .~(.! .<x>.; .<1>.)>.;;
                ^^^^^^^^
Error: .! error: 'cl not generalizable in ('cl, 'a) code
*)

(* CSP *)
let x = 1 in .<x>.;;
(*
- : ('cl, int) code = .<(* cross-stage persistent value (as id: x) *)>.
*)

let x = 1.0 in .<x>.;;
(*
- : ('cl, float) code = .<1.>.
*)

let l x = .<x>.;;                       (* polymorphic *)
(* val l : 'a -> ('cl, 'a) code = <fun> *)
l 1;;
(*
- : ('a, int) code = .<(* cross-stage persistent value (as id: x) *)>.
*)
l 1.0;;                  (* better printing in N100 *)
(*
- : ('a, float) code = .<1.>.
*)

.<List.rev>.;;
(*
- : ('cl, 'a list -> 'a list) code = .<List.rev>.
*)

.<Array.get>.;;
22: Stage for var is set to implicit 0:Array.get
(*
- : ('cl, 'a array -> int -> 'a) code = .<Array.get>.
*)
.<(+)>.;;
(*
22: Stage for var is set to implicit 0:Pervasives.+
- : ('cl, int -> int -> int) code = .<(+)>.
*)


