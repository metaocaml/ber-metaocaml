(* Various simple (one-liner) examples and NON-examples  *)
open Runcode;;

(* Safety of run *)

let tr1 = .<fun x -> .~(run .<.<1>.>.)>.;;
(*
val tr1 : ('a -> int) code = .<fun x_54  -> 1>. 
*)
let tr1' = .<fun x -> .~(run .<.<fun x -> x>.>.)>.;;
(*
val tr1' : ('a -> 'b -> 'b) code = .<fun x_55  x_56_57  -> x_56_57>. 
*)
let tr2 = .<fun x -> .~(let x = run .<1>. in .<x>.)>.;;
(*
val tr2 : ('a -> int) code = .<fun x_58  -> 1>. 
*)

let tr3 = .<fun x -> .~(let x = run .<x>. in .<x>.)>.;;

(*
Exception:
Failure
 "The code built at Characters 16-17:\n  let tr3 = .<fun x -> .~(let x = run .<x>. in .<x>.)>.;;\n                  ^\n is not closed: identifier x_59 bound at Characters 16-17:\n  let tr3 = .<fun x -> .~(let x = run .<x>. in .<x>.)>.;;\n                  ^\n is free".

Was:
  let tr3 = .<fun x -> .~(let x = !. .<x>. in .<x>.)>.;;
                                     ^^^^^
Error: !. error: 'a not generalizable in ('a, 'b) code
*)
print_endline "Error was expected";;

.< fun x -> .~ (run .< x >.) >.;;
(*
Exception:
Failure
 "The code built at Characters 7-8:\n  .< fun x -> .~ (run .< x >.) >.;;\n         ^\n is not closed: identifier x_60 bound at Characters 7-8:\n  .< fun x -> .~ (run .< x >.) >.;;\n         ^\n is free".

Was:
Characters 15-26:
  .< fun x -> .~ (!..< x >.) >.;;
                 ^^^^^^^^^^^
Error: !. occurs check error: 'cl occurs in ('cl, ('cl, 'a) code) code
*)
print_endline "Error was expected";;

let tr4 = .<fun x -> .~(let x = run x in .<x>.)>.;;
(*
Characters 35-36:
  let tr4 = .<fun x -> .~(let x = run x in .<x>.)>.;;
                                     ^
Error: Wrong level: variable bound at level 1 and used at level 0
*)
print_endline "Error was expected";;

let tr5 = .<fun x -> run .<1>.>.;;
(*
val tr5 : ('a -> int) code = .<fun x_61  -> Runcode.( run )  (.< 1  >.)>. 
*)
let 1 = (run tr5) true;;

let tr6 = .<fun x -> run .<x>.>.;;
(*
val tr6 : ('a -> 'a) code = .<fun x_62  -> Runcode.( run )  (.< x_62  >.)>. 
*)
let 1 = (run tr6) 1;;

let tr7 = .<fun x -> run x>.;;
(*
val tr7 : ('a code -> 'a) code = .<fun x_63  -> Runcode.( run )  x_63>. 

Was:
Characters 24-25:
  let tr7 = .<fun x -> !. x>.;;
                          ^
Error: !. error: 'a not generalizable in ('a, 'b) code
print_endline "Error was expected";;
*)
let 10 = run tr7 .<10>.;;

 (* Bizzare CSP *)
let tr8 = .<fun x -> .~(let y = .<x>. in .<y>.)>.;;
(*
Characters 43-44:
  let tr8 = .<fun x -> .~(let y = .<x>. in .<y>.)>.;;
                                             ^
Warning 22: The CSP value is a closure or too deep to serialize
val tr8 : ('a -> 'a code) code = .<fun x_121  -> (* CSP y *)>. 
*)
(* But it cannot be run! *)
let tr8r = run tr8;;
(*
val tr8r : '_a -> '_a code = <fun>

Was
Characters 14-17:
  let tr8r = !. tr8;;
                ^^^
Error: !. occurs check error: 'a occurs in ('a, 'b -> ('a, 'b) code) code
print_endline "Error was expected";;
*)
(* And it cannot be run indeed *)
run tr8 10;;
(*
- : int code = .<x_65>.

Failure("The code built at Characters 16-17:\n   is not closed: identifier x_65 bound at Characters 16-17:\n   is free")
*)
print_endline "Error was expected";;

let tm1 = .<fun x -> .< x >. >.;;
(*
val tm1 : ('a -> 'a code) code = .<fun x_66  -> .< x_66  >.>. 
*)
run tm1 10;;
(*
- : int code = .<(* CSP x_122 *) Obj.magic 10>. 
*)
let 10 = run (run tm1 10);;

(* Generalization *)

let tg1 = run ((fun x -> .<x>.) (ref []));;
(*
val tg1 : '_a list ref = {contents = []}
   should not be polymorphic!
*)
let tg2 = run .<ref []>.;;
(*
val tg2 : '_a list ref = {contents = []}
   should not be polymorphic!
*)

(*
(* First-class polymorphism *)

(* Recall, in runcode.mli:

type 'a cde = {cde : 'c. ('c,'a) code}  (* Type of the closed code *)

*)

(* In all previous versions of MetaOCaml, up to BER N004:

# Runcode.run;;
- : 'a Runcode.cde -> 'a = <fun>
# {Runcode.cde = .<1>.};;
- : int Runcode.cde = .<1>.
# Runcode.run {Runcode.cde = .<1>.};;
- : int = 1
# .<{Runcode.cde = .<1>.}>.;;
- : ('a, int Runcode.cde) code = .<{Runcode.cde = .<1>.}>.
# !. .<{Runcode.cde = .<1>.}>.;;
Characters 22-23:
  !. .<{Runcode.cde = .<1>.}>.;;
                        ^
Error: This expression has type ('a, int) code
       but an expression was expected of type ('b, int) code

Exception: Trx.TypeCheckingError.
*)

(* Now *)
let tfc1 = {Runcode.cde = .<1>.};;
(* - : int Runcode.cde = .<1>. *)
let 1 = Runcode.run {Runcode.cde = .<1>.};;

let tfc2 = .<{Runcode.cde = .<1>.}>.;;
(*
- : ('cl, int Runcode.cde) code = .<{Runcode.cde = .<1>.}>. 
*)
let tfc3 = !. .<{Runcode.cde = .<1>.}>.;;
(* - : int Runcode.cde = .<1>.  *)
let tfc4 = {Runcode.cde= .<{Runcode.cde = .<1>.}>.};;
(* - : int Runcode.cde Runcode.cde = .<{Runcode.cde = .<1>.}>.  *)
let tfc5 = Runcode.run {Runcode.cde= .<{Runcode.cde = .<1>.}>.};;
(* - : int Runcode.cde = .<1>.  *)
let 1 = Runcode.run (Runcode.run {Runcode.cde= .<{Runcode.cde = .<1>.}>.});;
(* - : int = 1 *)
*)

Printf.printf "\nAll Done\n";;
