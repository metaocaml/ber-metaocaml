(* Test for the maintenance of generalization levels.
   The test submitted by Jun Inoue, Apr 24, 2014

  Previously, when loaded in metaocaml top level,
  it triggered assertion false in
  typing/typecore.ml, in the local function lower_args
  defined when type checking Pexp_apply.
*)

let cde_eq c1 = ()

type 'a cde = Cde of 'a

let get_cde x =
  match x with
  | Cde c -> ()

type 'a glist = Cons of 'a


(* If we move glist_eq below the let _ = ... part, we get:

    | Cons x -> true
      ^^^^^^
Error: This pattern matches values of type 'a glist
       but a pattern was expected which matches values of type 'a glist
       The type constructor glist would escape its scope

 *)
let glist_eq x =
  match x with
  | Cons x -> ()

(* Invoking Runcode.run seems to be essential.  Defining and using
     let myrun : 'a code -> 'a = fun x -> failwith ""
   doesn't provoke the bug.
 *)
let _ = Runcode.run .<()>.;;

(* This has to use glist_eq.  Using get_cde instead won't cut it.  *)
let sumF env = glist_eq env
