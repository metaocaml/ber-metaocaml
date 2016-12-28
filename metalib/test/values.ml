(* Test of val_code type *)
open Runcode
;;

let _ = .<function () -> ()>.
(*
- : (unit -> unit) code = .<fun ()  -> ()>. 
*)
;;

let _ = .<function () -> ()>. [@metaocaml.value]
(*
- : (unit -> unit) Trx.val_code = <abstr>
*)
;;

let t:'a Trx.val_code = .<function () -> ()>. [@metaocaml.value]
(*
val t : (unit -> unit) Trx.val_code = <abstr>
*)

let _ = Print_code.code_of_val_code t
(*
- : (unit -> unit) code = .<fun ()  -> ()>. 
*)

let _ = .<(1,function () -> ())>. [@metaocaml.value]
(*
- : (int * (unit -> unit)) Trx.val_code = <abstr>
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
- : (unit -> int) Trx.val_code = <abstr>
*)
;;

let t = let x = "xxx" in .<x>. [@metaocaml.value]
(*
val t : string Trx.val_code = <abstr>
*)

let "xxx" = run (Print_code.code_of_val_code t)
;;
let _ = .<function x -> .~(let y = .<x>. in y)>. [@metaocaml.value]
(*
- : ('a -> 'a) Trx.val_code = <abstr>
*)
;;

let t = .<function x -> .~(let y = .<x>. [@metaocaml.value] in 
                               Print_code.code_of_val_code y)>. 
  [@metaocaml.value]

(*
    val t : ('a -> 'a) Trx.val_code = <abstr>
*)
;;
let 5 = run (Print_code.code_of_val_code t) 5
;;

let t = .<function x -> .~(let y = .<x+1>. [@metaocaml.value] in 
                               Print_code.code_of_val_code y)>. 
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
*)
let _ = .<(.~(.<1>.),2)>.
  [@metaocaml.value]
(*
- : (int * int) Trx.val_code = <abstr>
*)
;;

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
- : ((int * int) code * int) Trx.val_code = <abstr>
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
- : (int code * int) Trx.val_code = <abstr>
*)
;;

let _ = .<(.<1+1>.,2)>.
  [@metaocaml.functionliteral]
  [@metaocaml.value]
;;
(*
  Characters 8-23:
  let _ = .<(.<1+1>.,2)>.
          ^^^^^^^^^^^^^^^
Error: bracket is followed by inconsistent metaocaml attributes
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
print_endline "Error was expected";;

let _ = (.<1>. [@metaocaml.value],2);;
(*
- : int Trx.val_code * int = (<abstr>, 2)
*)

let t = .<(.<1>. [@metaocaml.value],2)>.
(*
val t : (int Trx.val_code * int) code = .<
  (.< ((1)[@metaocaml.value ])  >., 2)>. 
*)
let t1 = run t;;
(*
val t1 : int Trx.val_code * int = (<abstr>, 2)
*)
let 1 = run (Print_code.code_of_val_code @@ fst t1);;


(*
let _ = .<.~(.<1>.)>.
  [@metaocaml.value]
*)
;;

print_endline "\nAll done\n";;
