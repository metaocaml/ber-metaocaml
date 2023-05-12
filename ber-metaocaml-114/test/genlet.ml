(* Simple Tests of genlet (from MetaFX/metaocaml) *)
[@@@warning "-8"]
;;

(* First, local let-insertion *)

let t = letlv .<1+2>. (fun v -> .<4 + .~((v : 'a val_code :> 'a code))>.)
;;
(*
- : int code = .<let t_1 = 1 + 2 in 4 + t_1>. 
*)

let t = letl .<1+2>. (fun v -> .<4 + .~v>.)
;;
(*
- : int code = .<let t_2 = 1 + 2 in 4 + t_2>. 
*)

let t = letl ~name:"v" .<1+2>. (fun v -> .<4 + .~v>.)
;;
(*
val t : int code = .<let v_3 = 1 + 2 in 4 + v_3>. 
*)
let 7 = Runcode.run t

let t = letl ~name:"Abc!" .<1+2>. (fun v -> .<4 + .~v>.)
;;
let 7 = Runcode.run t

let t = letl .<3>. (fun v -> .<4 + .~v>.)
;;
(*
- : int code = .<4 + 3>. 
*)

let t = .<fun x -> .~(letl .<x>. @@ fun v -> .<4 + .~v>.)>.
;;
(*
- : (int -> int) code = .<fun x_4 -> 4 + x_4>. 
*)

let t = .<fun x -> .~(letl .<x+1>. @@ fun v -> .<4 + .~v>.)>.
;;
(*
- : (int -> int) code = .<fun x_5 -> let t_6 = x_5 + 1 in 4 + t_6>. 
*)

(* Now, real genlet *)
let _ = .<1 + .~(genletv .<2>. : 'a val_code :> 'a code)>.
;;
(* - : int code = .<1 + 2>.  *)

let _ = .<fun x -> x + .~(genletv .<2>. : 'a val_code :> 'a code)>.
;;
(*
- : (int -> int) code = .<fun x_2  -> x_2 + 2>. 
*)
;;

let t = .<fun x -> x + .~(genlet .<1+2>.)>.;;
;;
(*
val t : (int -> int) code = .<let lv_4 = 1 + 2  in fun x_3  -> x_3 + lv_4>. 
*)
let 6 = Runcode.run t 3;;

let t = .<fun x -> x + .~(genlet .<x>.)>.
;;
(*
val t : (int -> int) code = .<fun x_8  -> x_8 + x_8>. 
*)

let t = .<fun x -> x + .~(genlet .<let _y = 1 in x>.)>.
;;
(*
val t : (int -> int) code = .<
  fun x_8  -> let lv_10 = let _y_9 = 1  in x_8  in x_8 + lv_10>. 
*)

let t = .<fun x -> x + .~(genlet .<fun () -> x>.) ()>.
;;
(*
val t : (int -> int) code = .<
  fun x_11  -> let lv_12 () = x_11  in x_11 + (lv_12 ())>. 
*)
let 8 = Runcode.run t 4;;

let t = .<fun x -> x + .~(genlet .<fun x -> x + 2>.) x>.
;;
(*
val t : (int -> int) code = .<
  let lv_15 x_14 = x_14 + 2  in fun x_13  -> x_13 + (lv_15 x_13)>. 
*)
let 10 = Runcode.run t 4;;

let t = .<fun x -> x + .~(genlet .<x+1>.)>.
;;
(*
val t : (int -> int) code = .<
  fun x_9  -> let lv_10 = x_9 + 1  in x_9 + lv_10>. 
*)
let 9 = Runcode.run t 4;;

(* Multiple binders *)
let t = .<fun x y -> x + y + .~(genlet .<2+3>.)>.;;
(*
val t : (int -> int -> int) code = .<
  let lv_16 = 2 + 3  in fun x_14  -> fun y_15  -> (x_14 + y_15) + lv_16>. 
*)

let 10 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(genlet .<x>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_17  -> fun y_18  -> (x_17 + y_18) + x_17>. 
*)
let 6 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(genlet .<y>.)>.;;
;;
(*
val t : (int -> int -> int) code = .<
  fun x_19  -> fun y_20  -> (x_19 + y_20) + y_20>. 
*)
let 9 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(genlet .<x+1>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_21  -> let lv_23 = x_21 + 1  in fun y_22  -> (x_21 + y_22) + lv_23>. 
*)
let 7 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(genlet .<y+1>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_24  -> fun y_25  -> let lv_26 = y_25 + 1  in (x_24 + y_25) + lv_26>. 
*)
let 10 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(genlet .<y+1+x>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_27  ->
    fun y_28  -> let lv_29 = (y_28 + 1) + x_27  in (x_27 + y_28) + lv_29>.
  
*)
let 11 = Runcode.run t 1 4;;


(* Nested genlet *)
let t = .<fun x y -> x + y + 
  .~(genlet .<.~(genlet .<y+1+x>.)>.) >.;;
(* same as before: genlet is idempotent *)
let 11 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(genlet .<y+ .~(genlet .<1+2>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  let lv_39 = 1 + 2  in
  fun x_37  ->
    fun y_38  -> let lv_40 = y_38 + lv_39  in (x_37 + y_38) + lv_40>.
  
*)
let 12 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(genlet .<y+ .~(genlet .<1+x>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  fun x_41  ->
    let lv_43 = 1 + x_41  in
    fun y_42  -> let lv_44 = y_42 + lv_43  in (x_41 + y_42) + lv_44>.
  
*)
let 11 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(genlet .<y+ .~(genlet .<1+x+y>.)>.) >.;;
(*
  val t : (int -> int -> int) code = .<
  fun x_27  ->
    fun y_28  ->
      let lv_29 = (1 + x_27) + y_28  in
      let lv_30 = y_28 + lv_29  in (x_27 + y_28) + lv_30>.
  
*)
let 15 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(genlet .<x+ .~(genlet .<1+x+y>.)>.) >.;;
(*
  val t : (int -> int -> int) code = .<
  fun x_31  ->
    fun y_32  ->
      let lv_33 = (1 + x_31) + y_32  in
      let lv_34 = x_31 + lv_33  in (x_31 + y_32) + lv_34>.
  
*)
let 12 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(genlet .<1+ .~(genlet .<1+x+2>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  fun x_35  ->
    let lv_37 = (1 + x_35) + 2  in
    let lv_38 = 1 + lv_37  in fun y_36  -> (x_35 + y_36) + lv_38>.
  
*)
let 10 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(genlet .<1+ .~(genlet .<1+3+2>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  let lv_41 = (1 + 3) + 2  in
  let lv_42 = 1 + lv_41  in fun x_39  -> fun y_40  -> (x_39 + y_40) + lv_42>. 
*)
let 12 = Runcode.run t 1 4;;

(* Duplication *)
let t =
  let x = genlet .<1 + 2>. in
  .<.~x + .~x>.
;;
(*
  val t : int code = .<let lv_1 = 1 + 2  in lv_1 + lv_1>. 
*)
let 6 = Runcode.run t;;

let t =
  let x = genlet .<1 + 2>. in
  let y = genlet .<1 + .~x>. in
  .<.~x + .~y>.
;;
(*
    val t : int code = .<
  let lv_2 = 1 + 2  in let lv_3 = 1 + lv_2  in lv_2 + lv_3>. 
*)
let 7 = Runcode.run t;;

let t =
  let x = genlet .<1 + 2>. in
  let y = genlet .<1 + .~x>. in
  let z = genlet .<1 + .~y>. in
  .<.~x + .~z + .~y>.
(*
          val t : int code = .<
  let lv_4 = 1 + 2  in
  let lv_5 = 1 + lv_4  in let lv_6 = 1 + lv_5  in (lv_4 + lv_6) + lv_5>. 
*)
let 12 = Runcode.run t;;

let t =
 .<fun u ->
  .~(let x = genlet .<1 + 2>. in
     let y = genlet .<u + .~x>. in
     .<.~x + .~y>.)>.
;;
(*
    val t : (int -> int) code = .<
  let lv_8 = 1 + 2  in fun u_7  -> let lv_9 = u_7 + lv_8  in lv_8 + lv_9>. 
*)
let 8 = Runcode.run t 2;;

let t =
 .<fun u ->
  .~(let x = genlet .<u + 2>. in
     let y = genlet .<1 + .~x>. in
     .<.~x + .~y>.)>.
;;
let 9 = Runcode.run t 2;;

let t =
 .<fun u ->
  .~(let x = genlet .<u + 2>. in
     let y = genlet .<1 + .~x>. in
     let z = genlet .<1 + .~y>. in
     .<.~x + .~z + .~y + .~z>.)>.
;;
(*
      val t : (int -> int) code = .<
  fun u_11  ->
    let lv_12 = u_11 + 2  in
    let lv_13 = 1 + lv_12  in
    let lv_14 = 1 + lv_13  in ((lv_12 + lv_14) + lv_13) + lv_14>.
  
*)
let 21 = Runcode.run t 2;;



(* A simple DSL. See loop_motion_gen.ml for a realistic example *)
module type DSL = sig
  val sqr           : int code -> int code
  val make_incr_fun : (int code -> int code) -> (int -> int) code
end
;;
(* Sample DSL expressions *)
module DSLExp(S: DSL) = struct
  open S
  let exp1 = sqr .<2+3>.
  let exp2 = make_incr_fun (fun x -> sqr .<2+3>.)
  let exp3 = make_incr_fun (fun x -> sqr .<.~x + 3>.)
end
;;
(* The naive implementation of the DSL *)
module DSL1 = struct
  let sqr e = .<.~e * .~e>.
  let make_incr_fun body = .<fun x -> x + .~(body .<x>.)>.
end
;;
let module M = DSLExp(DSL1) in
  (M.exp1, M.exp2, M.exp3)
;;
(*
- : int code * (int -> int) code * (int -> int) code =
(.<(2 + 3) * (2 + 3)>. , 
 .<fun x_14  -> x_14 + ((2 + 3) * (2 + 3))>. , 
 .<fun x_15  -> x_15 + ((x_15 + 3) * (x_15 + 3))>. )
*)
let (25,35,179) =
 let module M = DSLExp(DSL1) in
   (Runcode.run M.exp1, Runcode.run M.exp2 10, 
    Runcode.run M.exp3 10)
;;

(* Adding let-insertion, transparently *)
module DSL2 = struct
  include DSL1
  let sqr e = DSL1.sqr @@ genlet e
end
;;
let module M = DSLExp(DSL2) in
  (M.exp1, M.exp2, M.exp3)
;;
(*
- : int code * (int -> int) code * (int -> int) code =
(.<let lv_19 = 2 + 3  in lv_19 * lv_19>. , .<
 let lv_21 = 2 + 3  in fun x_20  -> x_20 + (lv_21 * lv_21)>. , .<
 fun x_22  -> let lv_23 = x_22 + 3  in x_22 + (lv_23 * lv_23)>. )
*)

let (25,35,179) =
 let module M = DSLExp(DSL2) in
   (Runcode.run M.exp1, Runcode.run M.exp2 10, 
    Runcode.run M.exp3 10)
;;

(* Scope-restricted let-insertion *)

(* Showing why we need a restricted genlet. Look at the code generated
   for the following
 *)
let sum_up = 
  let body arr =
    letl .<ref 0>. @@ fun i -> 
    let arr = genlet arr in
    let sum = genlet ~name:"sum" .<ref 0>. in
    .<while ! .~i < Array.length .~arr do
       .~sum := ! .~sum + (.~arr).(! .~i);
      incr .~i
    done;
    ! .~sum>.
  in .<fun x -> .~(body .<x>.)>.
;;
(*
val sum_up : (int array -> int) code = .<
  let sum_9 = Stdlib.ref 0 in
  fun x_7 ->
    let t_8 = Stdlib.ref 0 in
    while (! t_8) < (Stdlib.Array.length x_7) do
      (sum_9 := ((! sum_9) + (Stdlib.Array.get x_7 (! t_8))); Stdlib.incr t_8)
      done;
    ! sum_9>.
*)  


let[@warning "-8"] 6 = Runcode.run 
    .<let sum_up = .~sum_up in sum_up [|1;2;3|]>.
;;
(* weird, eh? *)
let[@warning "-8"] 24 = Runcode.run 
    .<let sum_up = .~sum_up in sum_up [|1;2;3|] + sum_up [|4;5|]>.
;;

(* Fixed code; now sum is bound inside the function *)
let sum_up = 
  let body arr =
    with_locus @@ fun locus ->
    letl .<ref 0>. @@ fun i -> 
    let arr = genlet arr in
    let sum = genlet ~name:"sum" ~locus .<ref 0>. in
    .<while ! .~i < Array.length .~arr do
       .~sum := ! .~sum + (.~arr).(! .~i);
      incr .~i
    done;
    ! .~sum>.
  in .<fun x -> .~(body .<x>.)>.
;;

(*
val sum_up : (int array -> int) code = .<
  fun x_14 ->
    let sum_17 = Stdlib.ref 0 in
    let t_16 = Stdlib.ref 0 in
    while (! t_16) < (Stdlib.Array.length x_14) do
      (sum_17 := ((! sum_17) + (Stdlib.Array.get x_14 (! t_16)));
       Stdlib.incr t_16)
      done;
    ! sum_17>.
*)

let 6 = Runcode.run 
    .<let sum_up = .~sum_up in sum_up [|1;2;3|]>.
;;
let 15 = Runcode.run 
    .<let sum_up = .~sum_up in sum_up [|1;2;3|] + sum_up [|4;5|]>.
;;

(* Simpler tests, differing positions of locus *)
let t = .<fun x y -> x + y + .~(genlet .<x+1>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_93 -> let t_95 = x_93 + 1 in fun y_94 -> (x_93 + y_94) + t_95>. 
*)
let 7 = Runcode.run t 1 4;;

let t = .<fun x y -> 
  .~(with_locus @@ fun locus ->
    .<x + y + .~(genlet ~locus .<x+1>.)>.)>.
;;
(*
  val t : (int -> int -> int) code = .<
  fun x_96 -> fun y_97 -> let t_99 = x_96 + 1 in (x_96 + y_97) + t_99>. 
*)
let 7 = Runcode.run t 1 4;;

let t = .<fun x -> 
  .~(with_locus @@ fun locus ->
    .<fun y -> x + y + .~(genlet ~locus .<x+1>.)>.)>.
;;
(*
  val t : (int -> int -> int) code = .<
  fun x_100 -> let t_103 = x_100 + 1 in fun y_102 -> (x_100 + y_102) + t_103>.
*)
let 7 = Runcode.run t 1 4;;

(* Setting the locus too high *)
let t = .<fun x -> 
  .~(with_locus @@ fun locus ->
    .<fun y -> x + y + .~(genlet ~locus .<y+1>.)>.)>.
;;
(*
  val t : (int -> int -> int) code = .<
  fun x_104 -> fun y_106 -> let t_107 = y_106 + 1 in (x_104 + y_106) + t_107>.
*)
let 10 = Runcode.run t 1 4;;

let _ = print_endline "\nAll done\n";;
