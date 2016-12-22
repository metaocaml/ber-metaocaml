(* Test of the first-class pattern-match *)

let t = .<function () -> ()>.
;;

let t[@metaocaml.functionliteral] = .<function () -> ()>.
;;

let t = .<function () -> ()>. [@metaocaml.functionliteral]
;;

let t = .<let x = function () -> () in x>. [@metaocaml.functionliteral]
;;


(* check also the generalization *)
let t = .<fun x -> x>. [@metaocaml.functionliteral]
;;

let t = .<function [] -> true | (_::_) -> false>. [@metaocaml.functionliteral]

;;
