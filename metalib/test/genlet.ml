(* Simple Tests of genlet (from MetaFX/metaocaml) *)
open Print_code;;

let _ = .<1 + .~(code_of_val_code @@ genlet .<2>.)>.;;
(* - : int code = .<1 + 2>.  *)

let _ = .<fun x -> x + .~(code_of_val_code @@ genlet .<2>.)>.
(*
- : (int -> int) code = .<fun x_2  -> x_2 + 2>. 
*)
;;

let t = .<fun x -> x + .~(code_of_val_code @@ genlet .<1+2>.)>.
;;
(*
val t : (int -> int) code = .<let lv_4 = 1 + 2  in fun x_3  -> x_3 + lv_4>. 
*)
let 6 = Runcode.run t 3;;

let t = .<fun x -> x + .~(code_of_val_code @@ genlet .<x>.)>.
;;
(*
val t : (int -> int) code = .<fun x_8  -> x_8 + x_8>. 
*)

let t = .<fun x -> x + .~(code_of_val_code @@ genlet .<x+1>.)>.
;;
(*
val t : (int -> int) code = .<
  fun x_9  -> let lv_10 = x_9 + 1  in x_9 + lv_10>. 
*)
let 9 = Runcode.run t 4;;

(* Multiple binders *)
let t = .<fun x y -> x + y + .~(code_of_val_code @@ genlet .<2+3>.)>.;;
(*
val t : (int -> int -> int) code = .<
  let lv_16 = 2 + 3  in fun x_14  -> fun y_15  -> (x_14 + y_15) + lv_16>. 
*)

let 10 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(code_of_val_code @@ genlet .<x>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_17  -> fun y_18  -> (x_17 + y_18) + x_17>. 
*)
let 6 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(code_of_val_code @@ genlet .<y>.)>.;;
;;
(*
val t : (int -> int -> int) code = .<
  fun x_19  -> fun y_20  -> (x_19 + y_20) + y_20>. 
*)
let 9 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(code_of_val_code @@ genlet .<x+1>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_21  -> let lv_23 = x_21 + 1  in fun y_22  -> (x_21 + y_22) + lv_23>. 
*)
let 7 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(code_of_val_code @@ genlet .<y+1>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_24  -> fun y_25  -> let lv_26 = y_25 + 1  in (x_24 + y_25) + lv_26>. 
*)
let 10 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + .~(code_of_val_code @@ genlet .<y+1+x>.)>.;;
(*
val t : (int -> int -> int) code = .<
  fun x_27  ->
    fun y_28  -> let lv_29 = (y_28 + 1) + x_27  in (x_27 + y_28) + lv_29>.
  
*)
let 11 = Runcode.run t 1 4;;


(* Nested genlet *)
let t = .<fun x y -> x + y + 
  .~(code_of_val_code @@ genlet
  .<.~(code_of_val_code @@ genlet .<y+1+x>.)>.) >.;;
(* same as before: genlet is idempotent *)
let 11 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(code_of_val_code @@ genlet
  .<y+ .~(code_of_val_code @@ genlet .<1+2>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  let lv_39 = 1 + 2  in
  fun x_37  ->
    fun y_38  -> let lv_40 = y_38 + lv_39  in (x_37 + y_38) + lv_40>.
  
*)
let 12 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(code_of_val_code @@ genlet
  .<y+ .~(code_of_val_code @@ genlet .<1+x>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  fun x_41  ->
    let lv_43 = 1 + x_41  in
    fun y_42  -> let lv_44 = y_42 + lv_43  in (x_41 + y_42) + lv_44>.
  
*)
let 11 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(code_of_val_code @@ genlet
  .<y+ .~(code_of_val_code @@ genlet .<1+x+y>.)>.) >.;;
(*
  val t : (int -> int -> int) code = .<
  fun x_27  ->
    fun y_28  ->
      let lv_29 = (1 + x_27) + y_28  in
      let lv_30 = y_28 + lv_29  in (x_27 + y_28) + lv_30>.
  
*)
let 15 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(code_of_val_code @@ genlet
  .<x+ .~(code_of_val_code @@ genlet .<1+x+y>.)>.) >.;;
(*
  val t : (int -> int -> int) code = .<
  fun x_31  ->
    fun y_32  ->
      let lv_33 = (1 + x_31) + y_32  in
      let lv_34 = x_31 + lv_33  in (x_31 + y_32) + lv_34>.
  
*)
let 12 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(code_of_val_code @@ genlet
  .<1+ .~(code_of_val_code @@ genlet .<1+x+2>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  fun x_35  ->
    let lv_37 = (1 + x_35) + 2  in
    let lv_38 = 1 + lv_37  in fun y_36  -> (x_35 + y_36) + lv_38>.
  
*)
let 10 = Runcode.run t 1 4;;

let t = .<fun x y -> x + y + 
  .~(code_of_val_code @@ genlet
  .<1+ .~(code_of_val_code @@ genlet .<1+3+2>.)>.) >.;;
(*
val t : (int -> int -> int) code = .<
  let lv_41 = (1 + 3) + 2  in
  let lv_42 = 1 + lv_41  in fun x_39  -> fun y_40  -> (x_39 + y_40) + lv_42>. 
*)
let 12 = Runcode.run t 1 4;;

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

(* Adding let-insertion, trasparently *)
module DSL2 = struct
  include DSL1
  let sqr e = DSL1.sqr @@ code_of_val_code @@ genlet e
end
;;
let module M = DSLExp(DSL2) in
  (M.exp1, M.exp2, M.exp3)
;;
(*
- : int code * (int -> int) code * (int -> int) code =
(.<let lv_45 = 2 + 3  in let lv_45 = 2 + 3  in lv_45 * lv_45>. , .<
 let lv_47 = 2 + 3  in
 let lv_47 = 2 + 3  in fun x_46  -> x_46 + (lv_47 * lv_47)>. , .<
 fun x_48  ->
   let lv_49 = x_48 + 3  in let lv_49 = x_48 + 3  in x_48 + (lv_49 * lv_49)>.
 )
*)

let (25,35,179) =
 let module M = DSLExp(DSL2) in
   (Runcode.run M.exp1, Runcode.run M.exp2 10, 
    Runcode.run M.exp3 10)
;;

print_endline "\nAll done\n";;
