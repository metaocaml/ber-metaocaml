(* Various simple (one-liner) examples and NON-examples  *)

(* Safety of run *)

let tr1 = .<fun x -> .~(.! .<.<1>.>.)>.;;
(*
val tr1 : ('a, 'b -> int) code = .<fun x_36 -> 1>.
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

let tr4 = .<fun x -> .~(let x = .! x in .<x>.)>.;;
(*
Characters 35-36:
  let tr4 = .<fun x -> .~(let x = .! x in .<x>.)>.;;
                                     ^
Error: Wrong level: variable bound at level 1 and used at level 0
*)

let tr5 = .<fun x -> .! .<1>.>.;;
(*
val tr5 : ('a, 'b -> int) code = .<fun x_38 -> .!.<1>.>.
*)

let tr6 = .<fun x -> .! .<x>.>.;;
(*
val tr6 : ('a, 'b -> 'b) code = .<fun x_39 -> .!.<x_39>.>.
*)

let tr7 = .<fun x -> .! x>.;;
(*
Characters 24-25:
  let tr7 = .<fun x -> .! x>.;;
                          ^
Error: .! error: 'a not generalizable in ('a, 'b) code
*)

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
