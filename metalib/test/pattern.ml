(* Test of the first-class pattern-match *)
open Runcode
;;

let t = .<function () -> ()>.
(*
val t : (unit -> unit) code = .<fun ()  -> ()>. 
*)
;;

let t[@metaocaml.functionliteral] = .<function () -> ()>.
(*
val t : (unit -> unit) code = .<fun ()  -> ()>. 
*)
;;

let t = .<function () -> ()>. [@metaocaml.functionliteral]
(*
val t : (unit -> unit) Trx.pat_code = <abstr>
*)
;;

let t = .<let x = function () -> () in x>. [@metaocaml.functionliteral]
(*
  let t = .<let x = function () -> () in x>. [@metaocaml.functionliteral];;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression does not appear to be a functional literal as requested
*)
;;
print_endline "Error was expected";;


(* check also the generalization *)
let t = .<fun x -> x>. [@metaocaml.functionliteral]
(*
val t : ('a -> 'a) Trx.pat_code = <abstr>
*)
;;

let t = .<function [] -> true | (_::_) -> false>. [@metaocaml.functionliteral]
(*
val t : ('a list -> bool) Trx.pat_code = <abstr>
*)
;;

let _ = Print_code.make_match .<[]>. [t]
(*
- : bool code = .<match [] with | [] -> true | _::_ -> false>. 
*)
;;

let c = Print_code.make_match .<[2]>. 
  [.<fun []       -> 0>.   [@metaocaml.functionliteral];
   .<function [1] -> 0>.   [@metaocaml.functionliteral];
   .<fun [x]      -> x+1>. [@metaocaml.functionliteral];
   .<fun (x::y)   -> x+1>. [@metaocaml.functionliteral]
 ]

(*
val c : int code = .<
  match [2] with
  | [] -> 0
  | 1::[] -> 0
  | x_14::[] -> x_14 + 1
  | x_12::y_13 -> x_12 + 1>. 
*)

let 3 = run c
;;

(* mepty pattern *)
let c = Print_code.make_match .<[2]>. []

let _ = run c
;;

print_endline "Error was expected"

;;
print_endline "\nAll done\n";;
