(* Test for the unsoundness of GADT pattern-match within brackets.

It was first reported by Jeremy Yallop on Fri, 2 Dec 2016 16:35:58 +0000
and further discussed in correspondence with him.
*)

(* This declaration is in the file tgadt_decl.mli 
type _ t = T : string t
*)

open Tgadt_decl

let f : type a. a t option code -> a -> unit code =
  fun c x -> .< match .~c with
  | None -> ()
  | Some T -> .~(print_endline x; .<()>.) >.
;;
let _ = f .< None >. 0
