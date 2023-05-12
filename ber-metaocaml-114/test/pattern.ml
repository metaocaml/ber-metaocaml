(* Test of the first-class pattern-match *)
open Runcode
[@@@warning "-8"]
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

let t : _ pat_code = .<function () -> ()>. [@metaocaml.functionliteral]
(*
val t : (unit -> unit) pat_code = <abstr>
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
val t : ('a -> 'a) pat_code = <abstr>
*)
;;

let t : ('a list -> bool) pat_code
    = .<function [] -> true | (_::_) -> false>. [@metaocaml.functionliteral]
(*
val t : ('a list -> bool) pat_code = <abstr>
*)
;;

let _ = make_match .<[]>. [t]
(*
- : bool code = .<match [] with | [] -> true | _::_ -> false>. 
*)
;;

let c = make_match .<[2]>. 
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
let c = make_match .<[2]>. []

let _ = run c
;;

print_endline "Error was expected"
;;

(* Build a function to remove an element from an int list *)
let sremove : int -> (int list -> int list) code = fun x ->
    .<let[@warning "-8"] rec loop l = .~(make_match .<l>. [
        .<fun [] -> []>. [@metaocaml.functionliteral];
        .<function h::t when h = x -> loop t>. [@metaocaml.functionliteral];
        .<function h::t -> h :: loop t>. [@metaocaml.functionliteral];
        ])
    in loop>.
;;

let c = sremove 2
;;
(*
val c : (int list -> int list) code = .<
  let rec loop_1 l_2 =
    match l_2 with
    | [] -> []
    | h_5::t_6 when h_5 = 2 -> loop_1 t_6
    | h_3::t_4 -> h_3 :: (loop_1 t_4) in
  loop_1>. 
*)

let [1;3;4;3] = Runcode.run c [1;2;3;4;2;3;2]
;;

print_endline "\nAll done\n";;
