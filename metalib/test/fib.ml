(* Tests of genlet *)

(* Compare with the corresponding example accompanying the paper
   ``Shifting the Stage''
   There is no longer any need in delimited control
 *)

open Print_code
;;
type 'a val_code = 'a Trx.val_code
;;

let genlet : 'a code -> 'a val_code = fun cde -> failwith "na"

(* The original Gibonacci *)

let rec gib x y n =
  if n = 0 then x else 
  if n = 1 then y else
  gib x y (n-1) + gib x y (n-2) 
;;

let 8 = gib 1 1 5;;

(* Naively staged Gibonacci *)

let rec gibgen x y n =
  if n = 0 then x else 
  if n = 1 then y else
  .<.~(gibgen x y (n-1)) + .~(gibgen x y (n-2))>.
;;
(*
  val gibgen : int code -> int code -> int -> int code = <fun>
*)
let test_gibgen n = .<fun x y -> .~(gibgen .<x>. .<y>. n)>.;;
(*
val test_gibgen : int -> (int -> int -> int) code = <fun>
*)

let test_gibgen5 = test_gibgen 5;;
(*
val test_gibgen5 : (int -> int -> int) code = .<
  fun x_1  ->
    fun y_2  -> (((y_2 + x_1) + y_2) + (y_2 + x_1)) + ((y_2 + x_1) + y_2)>.
  
*)
(* There is a lot of duplicated code ... *)

let 8 = Runcode.run test_gibgen5 1 1;;



(* Gibonacci with open recursion *)

let gib x y self n =
  if n = 0 then x else 
  if n = 1 then y else
  self (n-1) + self (n-2)
;;

(* the simple y combinator *)
let rec y_simple f n = f (y_simple f) n
;;

let 8 = y_simple (gib 1 1) 5;;

(* Now add memoization *)

let rec lookup k = 
  function [] -> None
    | ((k',v')::t) -> if k = k' then Some v' else lookup k t
let ext s n v = (n,v) :: s
let empty () = []
;;

(* Memoising fix-point combinator
 *)
let y_memo f n = 
  let tableref = ref (empty ()) in
  let rec memo n = 
    match (lookup n !tableref) with
    | None -> let v = f memo n in (tableref := ext !tableref n v; v)
    | Some v -> v
  in f memo n
;;

let 34 = y_memo (gib 1 1) 8;;
let 1346269 = y_memo (gib 1 1) 30;;
(* without memoization it would've taken a while...*)


(* Staging ... *)

(* Note the dynamic arguments, x and y, are at the front, so that
   self has to deal with the static argument only.
   Somehow, we have to specify what is dynamic/static.
   Shifting argument may be considered as a primitive BTA.
   Far more sophisticated BTA can be designed (using abstract
   interpretation, for example). But not here. We want a simple
   example.
*)

let sgib x y self n =
  if n = 0 then x else 
  if n = 1 then y else
  .<.~(self (n-1)) + .~(self (n-2))>.
;;

let test_ss n = .<fun x y -> .~(y_simple (sgib .<x>. .<y>.) n)>.;;
(* exponential explosion is evident *)
let test_ss1 = test_ss 5;;
(*
val test_ss1 : (int -> int -> int) code = .<
  fun x_3  ->
    fun y_4  -> (((y_4 + x_3) + y_4) + (y_4 + x_3)) + ((y_4 + x_3) + y_4)>.
  
*)
let 8 = Runcode.run test_ss1 1 1;;

(* But the explosion also occurs if we use the memoizing combinator! *)
let test_ssm n = .<fun x y -> .~(y_memo (sgib .<x>. .<y>.) n)>.;;
let test_ssm1 = test_ssm 5;;
(*
 val test_ssm1 : ('a, int -> int -> int) code =
  .<fun x_1 ->
    fun y_2 -> ((((y_2 + x_1) + y_2) + (y_2 + x_1)) + ((y_2 + x_1) + y_2))>.
*)
(* exponential explosion is evident; (y_2 + x_1) is repeated thrice *)

let 8 = Runcode.run test_ssm1 1 1;;

(* Memoising values rather than arbitrary code *)

(* We can either write a staging memo. Or, we can use the same y_memo,
   but make sure sgib returns values. So, it will produce code in
   A-normal form
*)

let sgibv (x:int val_code) (y:int val_code) self n : int val_code =
  if n = 0 then x else 
  if n = 1 then y else
  genlet .<.~(code_of_val_code @@ self (n-1)) + 
           .~(code_of_val_code @@ self (n-2))>.
;;

(* we are using the same y_memo *)
let test_ssv n = .<fun x y -> 
  .~(code_of_val_code @@
     y_memo (sgibv (.<x>. [@metaocaml.value]) (.<y>. [@metaocaml.value])) n)>.;;

let test_ssv1 = test_ssv 5;;

;;
