(* Quick test of BER MetaOCaml. From PEPM09 and PEPM08 papers:

     Closing the Stage: From staged code to typed closures.
     Yukiyoshi Kameyama, Oleg Kiselyov, and Chung-chieh Shan

*)

(* ---------------------------------------------------------------------- *)
(* The power example, Sec 2 *)

let square x = x * x
let rec power : int -> ('a,int) code -> ('a,int) code = 
   fun n -> fun x ->
    if n = 0 then .<1>.
       else if n mod 2 = 0 
           then .< (*csp*)square .~(power (n/2) x)>.
           else .<.~x * .~(power (n-1) x)>.
;;
let power7 : int -> int =
  .! .<fun x -> .~(Printf.printf "power\n"; power 7 .<x>.)>.;;
(* "power" printed once *)

let (128, 2187) = (power7 2, power7 3);;
(* nothing is printed...
  val res : int * int = (128, 2187)
*)


(* ---------------------------------------------------------------------- *)
(* The ef example. *)

(* The source code *)
let ef = fun z -> .<fun x -> .~z + x>.;;
let ef1 = .<fun y -> .~(ef .<y>.)>.;;
(*
  val ef1 : ('a, int -> int -> int) code =
  .<fun y_1 -> fun x_2 -> (y_1 + x_2)>.
*)
let 5 = (.! ef1) 2 3;; (* 5 *)

let ef2 = .<fun x -> fun y -> .~(ef .<x*y>.)>.;;
(*
 val ef2 : ('a, int -> int -> int -> int) code =
  .<fun x_1 -> fun y_2 -> fun x_3 -> ((x_1 * y_2) + x_3)>.
*)
let 10 = (.! ef2) 2 3 4;; (* 10 *)

(* ---------------------------------------------------------------------- *)
(* The eta example. *)

let (5,10,34) = 
  let eta f = .<fun x -> .~(f .<x>.)>. in
    (.! .< fun y ->         
             .~(eta (fun z -> .< .~z + y   >.)) >.)
    2 3,
    (.! .< fun y -> fun w -> 
             .~(eta (fun z -> .< .~z + y*w >.)) >.)
    2 3 4,
    (.! .< fun x u -> 
             .~(eta (fun z -> .<fun y -> .~z + u*x*y >.)) >.)
    2 3 4 5
 ;;


(* ---------------------------------------------------------------------- *)
(* Cross-stage presistence *)

(* This example includes persistence of a code value, which we
 specifically exclude in the paper. *)

let cspe =
.<fun x -> .~(let u = .<x>. in 
     (.! .<fun y -> .<.~u>.>.) ()) >.;;

(*
    val cspe : ('a, 'b -> 'b) code = .<fun x_1 -> x_1>.
*)

let 42 = (.! cspe) 42;;

(* This CSP example does fit our restriction *)

let rec scspe x = .<(fun y -> x) (scspe 1)>.;;
(*
val scspe : int -> ('a, int) code = <fun>
# scspe 10;;
- : ('a, int) code =
.<((fun y_1 -> 10) (((* cross-stage persistent value (as id: scspe) *)) 1))>.
# .! (scspe 10);;
- : int = 10
*)

let 10 = .! (scspe 10);;

(* ---------------------------------------------------------------------- *)
(* Scope extrusion via mutable state *)

let extr = let x = ref .<1>. in
    let _ = .<fun v -> .~(x := .<v>.; .<()>.)>. in
    !x;;
(* It does type-check ...*)

(*
    val extr : ('a, int) code = .<v_1>.

    # .! extr ;;
    Unbound value v_1
    Exception: Trx.TypeCheckingError.
*)


(*
 *  In this example, we compute a staged power function while tracking how many
 *  multiplications the generated code performs.  This example demonstrates the
 *  utility of our environment-passing translation, in two ways.  First, it is
 *  easiest to write this example if we use a side effect such as mutable state
 *  in MetaOCaml, but such an extension (a piece of state of type int) has not
 *  been shown sound except through our translation.  Second, we can write this
 *  example in pure MetaOCaml (more awkwardly) using our translation.
 *
 *  Thanks to Olivier Danvy for suggesting this example.
 *)

let rec powerc = function
    | 0 -> (.<fun x -> 1>.,0)
    | 1 -> (.<fun x -> x>.,1)
    | n -> let (c,n1) = powerc (pred n) in
           (.<fun x -> (.~c x) * x>.,succ n1)
;;
(*
  val powerc : int -> ('a, int -> int) code * int = <fun>
*)

let test = powerc 5;;
(*
val test : ('a, int -> int) code * int =
  (.<fun x_5 ->
   (((fun x_4 ->
       (((fun x_3 ->
           (((fun x_2 -> (((fun x_1 -> x_1) x_2) * x_2)) x_3) * x_3)) x_4) *
         x_4)) x_5) * x_5)>.,
   5)
*)

let 32 = (.! (fst test)) 2;;
(*
val testc : int = 32
*)

let mul x y = .<.<.~.~x * .~.~y>.>.;;
(*
val mul :
  ('a, ('b, int) code) code ->
  ('a, ('b, int) code) code -> ('a, ('b, int) code) code = <fun>
*)

let rec powerd = function
    | 0 -> (.<fun x -> .<1>.>.,0)
    | 1 -> (.<fun x -> x>.,1)
    | n -> let (c,n1) = powerd (pred n) in
           (.<fun x -> .~(mul .<.~c x>. .<x>.)>.,succ n1)
;;
(*
 val powerd : int -> ('a, ('b, int) code -> ('b, int) code) code * int = <fun>
*)

let test1 () = powerd 5;;
(*
val test1 : ('a, ('_b, int) code -> ('_b, int) code) code * int =
  (.<fun x_5 ->
   .<(.~((fun x_4 ->
           .<(.~((fun x_3 ->
                   .<(.~((fun x_2 -> .<(.~((fun x_1 -> x_1) x_2) * .~x_2)>.)
                          x_3) * .~x_3)>.) x_4) * .~x_4)>.) x_5) * .~x_5)>.>.,
   5)
*)

let testd = .! (fst (powerd 5));;
let testdd = .<fun x -> .~(testd .<x>.)>.;;
(*
val testdd : ('_a, int -> int) code =
  .<fun x_1 -> ((((x_1 * x_1) * x_1) * x_1) * x_1)>.
*)

(* An attempt to write testd without overt use of multiple levels:
   no nested brackets.
*)
let one = .<1>.;;
let mul1 x y = .<.~x * .~y>.;;
let mull x y = .<mul1 .~x  .~y>.;;

let rec powerd1 = function
    | 0 -> (.<fun x -> one>.,0)
    | 1 -> (.<fun x -> x>.,1)
    | n -> let (c,n1) = powerd1 (pred n) in
           (.<fun x -> .~(mull .<.~c x>. .<x>.)>.,succ n1)
;;
(*
  val powerd1 : int -> ('a, ('b, int) code -> ('b, int) code) code * int = <fun>
*)

let test11 = powerd1 5;;
(*
val test11 : ('a, ('_b, int) code -> ('_b, int) code) code * int =
  (.<fun x_5 ->
   (((* cross-stage persistent value (as id: mul1) *))
     ((fun x_4 ->
        (((* cross-stage persistent value (as id: mul1) *))
          ((fun x_3 ->
             (((* cross-stage persistent value (as id: mul1) *))
               ((fun x_2 ->
                  (((* cross-stage persistent value (as id: mul1) *))
                    ((fun x_1 -> x_1) x_2) x_2)) x_3) x_3)) x_4) x_4)) x_5)
     x_5)>.,
   5)
*)

let testd1 = .! (fst (powerd1 5));;
let testdd1 = .<fun x -> .~(testd1 .<x>.)>.;;
(*
val testdd1 : ('_a, int -> int) code =
  .<fun x_1 -> ((((x_1 * x_1) * x_1) * x_1) * x_1)>.
*)


(* Meta-programming with delimited continuations *)
(* Writing an efficient specialized version of Gibonacci,
   without using any fix-point combinators, etc.
*)

open Printf;;

(* The original Gibonacci *)

let rec gib x y n =
  if n = 0 then x else 
  if n = 1 then y else
  gib x y (n-1) + gib x y (n-2) 
;;
let 8 = gib 1 1 5;;

(* Naively staged Gibonacci, to the statically known value of n *)

let rec gibgen x y n =
  if n = 0 then x else 
  if n = 1 then y else
  .<.~(gibgen x y (n-1)) + .~(gibgen x y (n-2))>.
;;
(* 
  val gibgen : ('a, int) code -> ('a, int) code -> int -> ('a, int) code =
  <fun>
*)
let test_gibgen n = .<fun x y -> .~(gibgen .<x>. .<y>. n)>.;;
(* val test_gibgen : int -> ('a, int -> int -> int) code = <fun> *)
let test_gibgen5 = test_gibgen 5;;
(*
val test_gibgen5 : ('a, int -> int -> int) code =
  .<fun x_1 ->
   fun y_2 -> ((((y_2 + x_1) + y_2) + (y_2 + x_1)) + ((y_2 + x_1) + y_2))>.
*)
let 8 = (.! test_gibgen5) 1 1;;

(* Clearly, the naive Gibonacci is inefficient. 
   The specialized code test_gibgen5 shows why:
   the computation (y_2 + x_1) is repeated thrice within such a short fragment
*)

(* To improve Gibonacci, we have to add memoization *)

(* First we define the abstract data types of memoization table 
   with integer keys *)

(* For the sake of the closest correspondence with circle-shift.elf,
   we use pairs to emulate 'a option data type. In the rest of the
   code, 'a maybe is an abstract data type.
*)
module Maybe :
 sig
   type 'a maybe
   val nothing   : 'a maybe
   val just      : 'a -> 'a maybe
   val ifnothing : 'a maybe -> bool
   val fromjust  : 'a maybe -> 'a
 end = struct
   type 'a maybe  = bool * (unit -> 'a)
   let nothing    = (true,  fun () -> failwith "nothing")
   let just x     = (false, fun () -> x)
   let ifnothing  = fst
   let fromjust x = snd x ()
end;;
open Maybe;;

module Memo :
 sig
   type 'a memo
   val empty  : 'a memo
   val lookup : int -> 'a memo -> 'a maybe
   val ext    : 'a memo -> int -> 'a -> 'a memo
 end = struct
   (* The following implementation uses functions, for compatibility
      with circle-shift.elf. The rest of the code does not depend
      on the implementation and can't even know it.
    *)
   type 'a memo = int -> 'a maybe
   let empty    = fun key -> nothing
   let lookup   = fun n table -> table n
   let ext      = fun table n v -> 
                     fun key -> if key = n then just v else table key
end;;
open Memo;;

(* we can write the standard, textbook memoizer *)
(* It memoizes the result of the application of function f to the integer n.
 *)

let new_memo () =
  let table = ref empty in
  fun f n ->
    let r = lookup n !table in
    if ifnothing r 
    then                                (* memo table miss *)
      let v = f n in			(* compute the value *)
      table := ext !table n v; v
    else fromjust r			(* else return the memoized value *)
;;


(* Now we can memoize Gibonacci and obtain an improved version *)
let gibo x y =
  let memo = new_memo () in
  let rec loop n =
    if n = 0 then x else 
    if n = 1 then y else
    memo loop (n-1) + memo loop (n-2)
 in loop
;;

let 8  = gibo 1 1 5;;  (* 8 *)
let 1346269 = gibo 1 1 30;;
(* 1346269, without memoization it would've taken a while...*)

(* We may try to stage it, naively *)

let sgibo_naive x y =
  let memo = new_memo () in
  let rec loop n =
    if n = 0 then x else 
    if n = 1 then y else
    .<.~(memo loop (n-1)) + .~(memo loop (n-2))>.
 in loop
;;

let test_sgibo_naive5  = 
  .<fun x y -> .~(sgibo_naive .<x>. .<y>. 5)>.;;
(*
  val test_sgibo_naive5 : ('a, int -> int -> int) code =
  .<fun x_1 ->
   fun y_2 -> ((((y_2 + x_1) + y_2) + (y_2 + x_1)) + ((y_2 + x_1) + y_2))>.
*)

(* Alas, the result shows the duplication of computations. The result of
  loop, in sgibo_naive, is a present-stage value but future-stage
  computation. We saved effort at the present stage but we saved no
  computation at the future stage. We need let insertion to save 
  future-stage computations.
*)

(* But the let-insertion isn't that easy! The naive version *)

let sgibo1_naive x y =
  let memo = new_memo () in
  let rec loop n =
    if n = 0 then x else 
    if n = 1 then y else
    .<let t1 = .~(memo loop (n-1)) and t2 = .~(memo loop (n-2))
      in t1 + t2>.
 in loop
;;
let test_sgibo1_naive5  = 
  .<fun x y -> .~(sgibo1_naive .<x>. .<y>. 5)>.;;
 
(*
  val test_sgibo1_naive5 : ('a, int -> int -> int) code =
  .<fun x_1 ->
   fun y_2 ->
    let t1_3 =
     let t1_5 =
      let t1_7 = let t1_9 = y_2 and t2_10 = x_1 in (t1_9 + t2_10)
      and t2_8 = y_2 in
      (t1_7 + t2_8)
     and t2_6 = let t1_9 = y_2 and t2_10 = x_1 in (t1_9 + t2_10) in
     (t1_5 + t2_6)
    and t2_4 =
     let t1_7 = let t1_9 = y_2 and t2_10 = x_1 in (t1_9 + t2_10)
     and t2_8 = y_2 in
     (t1_7 + t2_8) in
    (t1_3 + t2_4)>.
*)
(* the naive version obviously doesn't do any good: It creates even bigger
   duplicated computations *)
  
(* We have to change the memo table implementation. Our memo table should
   contain only those future-stage computations that are future-stage
   values. So, we need to do let-insertion after we detected a miss.
   But for that, we have to re-write everything in CPS. We have to write
   the memo-table implementation in CPS:
*)
   
let new_memo_let_CPS () =
  let table = ref empty in
  fun f n k ->
    let r = lookup n !table in
    if ifnothing r 
    then                                (* memo table miss *)
      f n 				(* compute the value *)
       (fun v -> .<let t = .~v in 
                     .~(table := ext !table n .<t>.; k .<t>.)>.)
    else k (fromjust r)			(* else return the memoized value *)
;;

(* but we also must re-write sgibo in CPS! *)

let sgibo_CPS x y =
  let memo = new_memo_let_CPS () in
  let rec loop n k =
    if n = 0 then k x else 
    if n = 1 then k y else
    memo loop (n-1) (fun r1 ->
    memo loop (n-2) (fun r2 ->
    k .<.~r1 + .~r2>.))
  in loop
;;

let test_sgibo_CPS5  = 
  .<fun x y -> .~(sgibo_CPS .<x>. .<y>. 5 (fun x ->x))>.;;

(*
  val test_sgibo_CPS5 : ('a, int -> int -> int) code =
  .<fun x_1 ->
   fun y_2 ->
    let t_3 = y_2 in
    let t_4 = x_1 in
    let t_5 = (t_3 + t_4) in
    let t_6 = (t_5 + t_3) in let t_7 = (t_6 + t_5) in (t_7 + t_6)>.
*)
let 8 = (.! test_sgibo_CPS5) 1 1;;

(* Now we get the desired result: no duplicate computations.
   At the cost of changing all of our code, even sgibo, in CPS.
   Memoization is no longer easy -- it becomes very intrusive.
*)

(* Not only this approach inconvenient, it is also unsafe.
   The mutation in maintaining the table in new_memo_let_CPS
   results in unsafety. We store in the `global' memo table code
   values like .<t>. -- with variables bound in the scope 
   that is more narrow than the dynamic scope of the table.
 *)

(* Let's make a simple `pessimization' of sgibo1_CPS. Let's suppose the
   programmer didn't want to rewrite gib in CPS, and continued to use
   memoization in `direct style'.
*)

let sgibo1_bad x y =
  let memo = new_memo_let_CPS () in
  let rec loop n =
    if n = 0 then x else 
    if n = 1 then y else
    .<.~(memo (fun n k -> k (loop n)) (n-1) (fun x ->x)) + 
      .~(memo (fun n k -> k (loop n)) (n-2) (fun x ->x))>.
 in loop
;;

let test_sgibo1_bad  = 
  .<fun x y -> .~(sgibo1_bad .<x>. .<y>. 5)>.;;
(*
  val test_sgibo1_bad : ('a, int -> int -> int) code =
  .<fun x_1 ->
   fun y_2 ->
    (let t_7 = (t_6 + t_5) in t_7 +
      let t_6 =
       (let t_5 = (t_3 + let t_4 = x_1 in t_4) in t_5 + let t_3 = y_2 in t_3) in
      t_6)>.
*)

(* Although the result appears efficient -- only four additions --
   it is incorrect! Please notice how variable t_6 is referenced before
   it is bound. Attempting to run this code gives

.! test_sgibo1_bad;;
  Unbound value t_6
  Exception: Trx.TypeCheckingError.
*)

(* To rely on MetaOCaml's type soundness, we must not use any side effects
   in our code generator.  We could write our memoizing gib without state,
   by including state-passing in our continuation-passing, as follows.
*)

let new_memo_let_CPS_only f n k table =
   let r = lookup n table in
   if ifnothing r
   then
     f n
      (fun v table -> .<let t = .~v in
                    .~(k .<t>. (ext table n .<t>.))>.)
      table
   else
     k (fromjust r) table
;;

let sgibo_CPS_only x y =
  let memo = new_memo_let_CPS_only in
  let rec loop n k =
    if n = 0 then k x else 
    if n = 1 then k y else
    memo loop (n-1) (fun r1 ->
    memo loop (n-2) (fun r2 ->
    k .<.~r1 + .~r2>.))
  in loop
;;

let test_sgibo_CPS_only5  = 
  .<fun x y -> .~(sgibo_CPS_only .<x>. .<y>. 5 (fun r table -> r) empty)>.;;

let 8 = (.! test_sgibo_CPS_only5) 1 1;;

Printf.printf "\nAll Done\n";;
