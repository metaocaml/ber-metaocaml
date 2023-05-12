(* Test of val_code type *)
open Runcode
[@@@warning "-8"]
;;

let _ = .<function () -> ()>.
(*
- : (unit -> unit) code = .<fun ()  -> ()>. 
*)
;;

let _ = .<function () -> ()>. [@metaocaml.value]
(*
- : (unit -> unit) val_code = <abstr>
*)
;;

let t:'a val_code = .<function () -> ()>. [@metaocaml.value]
(*
val t : (unit -> unit) val_code = <abstr>
*)

let _ = (t : 'a val_code :> 'a code)
(*
- : (unit -> unit) code = .<fun ()  -> ()>. 
*)

let _ = .<(1,function () -> ())>. [@metaocaml.value]
(*
- : (int * (unit -> unit)) val_code = <abstr>
*)

let _ = .<(1+1,function () -> ())>. [@metaocaml.value]
;;
(*
Characters 10-33:
  let _ = .<(1+1,function () -> ())>. [@metaocaml.value]
            ^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression does not appear to be a syntactically a value as requested
*)
print_endline "Error was expected";;

let _ = .<(function () -> ()) ()>. [@metaocaml.value]
;;
(*
Characters 10-32:
  let _ = .<(function () -> ()) ()>. [@metaocaml.value]
            ^^^^^^^^^^^^^^^^^^^^^^
Error: The expression does not appear to be a syntactically a value as requested
*)
print_endline "Error was expected";;

let _ = .<function () -> 1+1>. [@metaocaml.value]
(*
- : (unit -> int) val_code = <abstr>
*)
;;

let t = let x = "xxx" in .<x>. [@metaocaml.value]
(*
val t : string val_code = <abstr>
*)

let "xxx" = run (t : 'a val_code :> 'a code)
;;
let _ = .<function x -> .~(let y = .<x>. in y)>. [@metaocaml.value]
(*
- : ('a -> 'a) val_code = <abstr>
*)
;;

let t = .<function x -> .~(let y = .<x>. [@metaocaml.value] in 
                               (y : 'a val_code :> 'a code))>. 
  [@metaocaml.value]

(*
val t : ('a -> 'a) val_code = .<fun x_6 -> x_6>. 
*)
;;
let 5 = run (t : 'a val_code :> 'a code) 5
;;

let t = .<function x -> .~(let y = .<x+1>. [@metaocaml.value] in 
                               code_of_val_code y)>. 
  [@metaocaml.value]
;;
(*
      Characters 37-40:
  let t = .<function x -> .~(let y = .<x+1>. [@metaocaml.value] in 
                                       ^^^
Error: The expression does not appear to be a syntactically a value as requested
*)
print_endline "Error was expected";;


(* This is correct: escape immediately followed by bracket essentailly
   cancels
   This is controversial
*)
let _ = .<(.~(.<1>.),2)>.
  [@metaocaml.value]
(*
- : (int * int) val_code = .<(1, 2)>. 
*)
;;
print_endline "Error can be expected";;

(* Now the error is emitted *)
let _ = .<(.~(let x = .<1>. in x),2)>.
  [@metaocaml.value]
(*Characters 10-36:
  let _ = .<(.~(let x = .<1>. in x),2)>.
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression does not appear to be a syntactically a value as requested
*)
;;
print_endline "Error was expected";;


let _ = .<(.<(3,.~(let x = .<1>. in x))>.,2)>.
  [@metaocaml.value]
;;
(*
- : ((int * int) code * int) val_code = .<
(.< (3, .~(let x_7 = .< 1  >. in x_7))  >., 2)>. 
*)

let _ = .<(.<(3,.~(.~(let x = .<1>. in .<x>.)))>.,2)>.
  [@metaocaml.value]
;;
(*
  Characters 10-52:
  let _ = .<(.<(3,.~(.~(let x = .<1>. in .<x>.)))>.,2)>.
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression does not appear to be a syntactically a value as requested
*)
print_endline "Error was expected";;


let _ = .<(.<1+1>.,2)>.
  [@metaocaml.value]
(*
- : (int code * int) val_code = .<(.< 1 + 1  >., 2)>. 
*)
;;

let _ = .<(.<1+1>.,2)>.
  [@metaocaml.functionliteral]
  [@metaocaml.value]
;;
(*
    Line 1, characters 8-23:
1 | let _ = .<(.<1+1>.,2)>.
            ^^^^^^^^^^^^^^^
Error: Invalid/unexpected attribute on a bracket
*)
print_endline "Error was expected";;

let _ = (.<1+1>.,2)
  [@metaocaml.value]
;;
(*
Characters 24-39:
    [@metaocaml.value]
      ^^^^^^^^^^^^^^^
Error: attribute metaocaml.value is misplaced. It must follow the closing bracket
*)
print_endline "Error can be expected";;

let _ : int val_code * int = (.<1>. [@metaocaml.value],2);;
(*
- : int val_code * int = (.<1>. , 2)
*)

(* Does the attribute propagate or not? *)
let t : (int val_code * int) code = .<(.<1>. [@metaocaml.value],2)>.
(*
val t : (int val_code * int) code = .<(((.< 1  >.)[@metaocaml.value ]), 2)>. 
*)
let t1 = run t;;
(*
val t1 : int val_code * int = (.<1>. , 2)
*)
let 1 = run (fst t1 : 'a val_code :> 'a code);;


(*
let _ = .<.~(.<1>.)>.
  [@metaocaml.value]
*)
;;

print_endline "\nAll done\n";;
