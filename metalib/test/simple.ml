(* Various simple (one-liner) examples and NON-examples  *)

(* Safety of run *)

let tr1 = .<fun x -> .~(.! .<.<1>.>.)>.;;
(*
val tr1 : ('a, 'b -> int) code = .<fun x_36 -> 1>.
*)
let tr1' = .<fun x -> .~(.! .<.<fun x -> x>.>.)>.;;
(*
val tr1' : ('cl, 'a -> 'b -> 'b) code = .<fun x_2 -> fun x_3_4 -> x_3_4>. 
*)
let tr2 = .<fun x -> .~(let x = .! .<1>. in .<x>.)>.;;
(*
val tr2 : ('a, 'b -> int) code = .<fun x_37 -> 1>.
*)

let tr3 = .<fun x -> .~(let x = .! .<x>. in .<x>.)>.;;

(*
  let tr3 = .<fun x -> .~(let x = .! .<x>. in .<x>.)>.;;
                                     ^^^^^
Error: .! error: 'a not generalizable in ('a, 'b) code
*)
print_endline "Error was expected";;

.< fun x -> .~ (.!.< x >.) >.;;
(*
Characters 15-26:
  .< fun x -> .~ (.!.< x >.) >.;;
                 ^^^^^^^^^^^
Error: .! occurs check error: 'cl occurs in ('cl, ('cl, 'a) code) code
*)
print_endline "Error was expected";;

let tr4 = .<fun x -> .~(let x = .! x in .<x>.)>.;;
(*
Characters 35-36:
  let tr4 = .<fun x -> .~(let x = .! x in .<x>.)>.;;
                                     ^
Error: Wrong level: variable bound at level 1 and used at level 0
*)
print_endline "Error was expected";;

let tr5 = .<fun x -> .! .<1>.>.;;
(*
val tr5 : ('a, 'b -> int) code = .<fun x_38 -> .!.<1>.>.
*)
let 1 = (.! tr5) true;;

let tr6 = .<fun x -> .! .<x>.>.;;
(*
val tr6 : ('a, 'b -> 'b) code = .<fun x_39 -> .!.<x_39>.>.
*)
let 1 = (.! tr6) 1;;

let tr7 = .<fun x -> .! x>.;;
(*
Characters 24-25:
  let tr7 = .<fun x -> .! x>.;;
                          ^
Error: .! error: 'a not generalizable in ('a, 'b) code
*)
print_endline "Error was expected";;

 (* Bizzare CSP *)
let tr8 = .<fun x -> .~(let y = .<x>. in .<y>.)>.;;
(*
val tr8 : ('a, 'b -> ('a, 'b) code) code =
  .<fun x_41 -> (* cross-stage persistent value (as id: y) *)>.
*)
(* But it cannot be run! *)
let tr8r = .! tr8;;
(*
Characters 14-17:
  let tr8r = .! tr8;;
                ^^^
Error: .! occurs check error: 'a occurs in ('a, 'b -> ('a, 'b) code) code
*)
print_endline "Error was expected";;


let tm1 = .<fun x -> .< x >. >.;;
(*
val tm1 : ('a, 'b -> ('c, 'b) code) code = .<fun x_35 -> .<x_35>.>.
*)

(* Generalization *)

let tg1 = .! ((fun x -> .<x>.) (ref []));;
(*
val tg1 : '_a list ref = {contents = []}
   should not be polymorphic!
*)
let tg2 = .! .<ref []>.;;
(*
val tg2 : '_a list ref = {contents = []}
   should not be polymorphic!
*)

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
# .! .<{Runcode.cde = .<1>.}>.;;
Characters 22-23:
  .! .<{Runcode.cde = .<1>.}>.;;
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
let tfc3 = .! .<{Runcode.cde = .<1>.}>.;;
(* - : int Runcode.cde = .<1>.  *)
let tfc4 = {Runcode.cde= .<{Runcode.cde = .<1>.}>.};;
(* - : int Runcode.cde Runcode.cde = .<{Runcode.cde = .<1>.}>.  *)
let tfc5 = Runcode.run {Runcode.cde= .<{Runcode.cde = .<1>.}>.};;
(* - : int Runcode.cde = .<1>.  *)
let 1 = Runcode.run (Runcode.run {Runcode.cde= .<{Runcode.cde = .<1>.}>.});;
(* - : int = 1 *)

Printf.printf "\nAll Done\n";;
