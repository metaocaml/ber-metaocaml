(* Trivial tests of MetaOCaml, which are also regression tests *)

3+2;;
(* - : int = 5 *)
let rec fact = function | 0 -> 1 | n -> n * fact (n-1);;
(* val fact : int -> int = <fun> *)
fact 5;;
(* - : int = 120 *)

.<1>.;;
(* - : ('cl, int) code = .<1>. *)
.<"aaa">.;;
(* - : ('cl, string) code = .<"aaa">. *)
.! .<1>.;;
(* - : int = 1 *)


.<fun x -> .~(let y = x in y)>.;;
(*
Characters 22-23:
  .<fun x -> .~(let y = x in y)>.;;
                        ^
Error: Wrong level: variable bound at level 1 and used at level 0
*)

.<fun x -> 1 + .~(.<true>.)>.;;
(*
Characters 20-24:
  .<fun x -> 1 + .~(.<true>.)>.;;
                      ^^^^
Error: This expression has type bool but an expression was expected of type
         int
*)

.<fun x -> .~(.! .<x>.; .<1>.)>.;;
(*
Characters 14-22:
  .<fun x -> .~(.! .<x>.; .<1>.)>.;;
                ^^^^^^^^
Error: .! error: 'cl not generalizable in ('cl, 'a) code
*)

(* CSP *)

let x = 1 in .<x>.;;
(*
- : ('cl, int) code = .<1>.
*)
let x = 1.0 in .<x>.;;
(*
- : ('cl, float) code = .<1.>.
*)
let x = true in .<x>.;;
(*
- : ('cl, bool) code = .<(true)>.
*)
let x = "aaa" in .<x>.;;
(*
- : ('cl, string) code = .<"aaa">.
*)
let x = 'a' in .<x>.;;
(*
- : ('cl, char) code = .<'a'>.
*)
let x = ['a'] in .<x>.;;
(*
- : ('cl, char list) code = .<(* cross-stage persistent value (as id: x) *)>.
*)

let l x = .<x>.;;                       (* polymorphic *)
(* val l : 'a -> ('cl, 'a) code = <fun> *)
l 1;;
(*
- : ('a, int) code = .<(* cross-stage persistent value (as id: x) *)>.
*)
l 1.0;;                  (* better printing in N100 *)
(*
- : ('a, float) code = .<1.>.
*)

.<List.rev>.;;
(*
- : ('cl, 'a list -> 'a list) code = .<List.rev>.
*)

.<Array.get>.;;
22: Stage for var is set to implicit 0:Array.get
(*
- : ('cl, 'a array -> int -> 'a) code = .<Array.get>.
*)
.<(+)>.;;
(*
22: Stage for var is set to implicit 0:Pervasives.+
- : ('cl, int -> int -> int) code = .<(+)>.
*)


let x = true in .<assert x>.;;
(*
- : ('cl, unit) code = .<assert (true)>.
*)

(* Applications and labels *)
.<succ 1>.;;
(*
- : ('cl, int) code = .<succ 1>.
*)

! .<succ 1>.;;
(* - : int = 2 *)

.<1 + 2>.;;
(*
- : ('cl, int) code = .<(1 + 2)>.
*)
.! .<(1 + 2)>.;;
(* - : int = 3 *)

.<String.length "abc">.;;
(*
- : ('cl, int) code = .<String.length "abc">.
*)
.! .<String.length "abc">.;;
(* - : int = 3 *)

.<StringLabels.sub ?pos:1 ?len:2 "abc">.;;
(*
- : ('cl, string) code = .<(StringLabels.sub "abc" ~pos:1 ~len:2>.
*)
.! .<StringLabels.sub ?pos:1 ?len:2 "abc">.;;
(* - : string = "bc" *)

.<StringLabels.sub ~len:2 ~pos:1 "abc">.;;
(*
- : ('cl, string) code = .<(StringLabels.sub "abc" ~pos:1 ~len:2>.
*)
.! .<StringLabels.sub ~len:2 ~pos:1 "abc">.;;
(* - : string = "bc" *)

(* Nested brackets and escapes and run *)
.<.<1>.>.;;
(*
- : ('cl, ('cl0, int) code) code = .<.<1>.>.
*)
.! .<.<1>.>.;;
(* - : ('cl, int) code = .<1>. *)
.! (.! .<.<1>.>.);;)
(* - : int = 1 *)
.<.!.<1>.>.;;
(*
- : ('cl, int) code = .<.!.<1>.>.
*)
.! .<.!.<1>.>.;;
(* - : int = 1 *)
.<1 + .~(let x = 2 in .<x>.)>.;;
(*
- : ('cl, int) code = .<(1 + 2)>.
*)
let x = .< 2 + 4 >. in .< .~ x + .~ x >. ;;
(*
- : ('cl, int) code = .<((2 + 4) + (2 + 4))>.
*)

.<1 + .~(let x = 2 in .<.<x>.>.)>.;;
(*
Characters 24-29:
  .<1 + .~(let x = 2 in .<.<x>.>.)>.;;
                          ^^^^^
Error: This expression has type ('cl, 'a) code
       but an expression was expected of type int
*)
.<1 + .! .~(let x = 2 in .<.<x>.>.)>.;;
(*
- : ('cl, int) code = .<(1 + .!.<2>.)>.
*)
.! .<1 + .! .~(let x = 2 in .<.<x>.>.)>.;;
(* - : int = 3 *)
.! .<1 + .~ (.~(let x = 2 in .<.<x>.>.))>.;;
(*
Characters 12-40:
  .! .<1 + .~ (.~(let x = 2 in .<.<x>.>.))>.;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong level: escape at level 0
*)

.<.<.~(.<1>.)>.>.;;
(*
- : ('cl, ('cl0, int) code) code = .<.<.~(.<1>.)>.>.
*)
.!.<.<.~(.<1>.)>.>.;;
(*
- : ('cl, int) code = .<1>.
*)
.<.<.~.~(.<.<1>.>.)>.>.;;
(*
- : ('cl, ('cl0, int) code) code = .<.<.~(.<1>.)>.>.
*)

(* Lazy *)
.<lazy 1>.;;
(*
- : ('cl, int lazy_t) code = .<lazy 1>.
*)
Lazy.force (.! .<lazy 1>.);;
(* - : int = 1 *)

(* Tuples *)
.<(1,"abc")>.;;
(*
- : ('cl, int * string) code = .<((1), ("abc"))>.
*)
.<(1,"abc",'d')>.;;
(*
- : ('cl, int * string * char) code = .<((1), ("abc"), ('d'))>.
*)
match .! .<(1,"abc",'d')>. with (_,x,_) -> x;;
(* - : string = "abc" *)

(* Arrays *)
.<[||]>.;;
(*
- : ('cl, 'a array) code = .<[||]>.
*)
let x = .<1+2>. in .<[|.~x;.~x|]>.;;
(*
- : ('cl, int array) code = .<[|(1 + 2); (1 + 2)|]>.
*)

(* Constructors and enforcing externality *)
.<raise Not_found>.;;
(*
- : ('cl, 'a) code = .<(raise (Not_found)>.
*)
.<raise (Scan_failure "")>.;;
(*
Characters 8-25:
  .<raise (Scan_failure "")>.;;
          ^^^^^^^^^^^^^^^^^
Error: Unbound constructor Scan_failure
*)
.<raise (Scanf.Scan_failure "")>.;;
(*
- : ('cl, 'a) code = .<(raise (Scanf.Scan_failure (""))>.
*)
open Scanf;;
.<raise (Scan_failure "")>.;;
(*
- : ('cl, 'a) code = .<(raise (Scanf.Scan_failure (""))>.
*)
.! .<raise (Scan_failure "")>.;;
(*
Exception: Scanf.Scan_failure "".
*)


.<true>.;;
(*
- : ('cl, bool) code = .<(true)>.
*)
.<Some 1>.;;
(*
- : ('cl, int option) code = .<(Some (1))>.
*)
.<Some [1]>.;;
(*
- : ('cl, int list option) code = .<(Some ([1]))>.
*)
.! .<Some [1]>.;;
(*
- : int list option = Some [1]
*)
.<None>.;;
(*
- : ('cl, 'a option) code = .<(None)>.
*)
.! .<None>.;;
(*
- : 'a option = None
*)

.<Genlex.Int 1>.;;
(*
- : ('cl, Genlex.token) code = .<(Genlex.Int (1))>.
*)
open Genlex;;
.<Int 1>.;;
(*
- : ('cl, Genlex.token) code = .<(Genlex.Int (1))>.
*)
.! .<Int 1>.;;
(*
- : Genlex.token = Int 1
*)

module Foo = struct exception E end;;
.<raise Foo.E>.;;
(*
Fatal error: exception Trx.TrxError("Exception Foo.E cannot be used within brackets. Put into a separate file.")
*)
type foo = Bar;;
.<Bar>.;;
(*
Fatal error: exception Trx.TrxError("Constructor Bar cannot be used within brackets. Put into a separate file.")
*)
module Foo = struct type foo = Bar end;;
.<Foo.Bar>.;;
(*
Fatal error: exception Trx.TrxError("Constructor Foo.Bar cannot be used within brackets. Put into a separate file.")
*)

(* Records *)

.<{Complex.re = 1.0; im = 2.0}>.;;
(*
- : ('cl, Complex.t) code = .<{Complex.re = 1.0; Complex.im = 2.0}>.
*)
Complex.conj (.! .<{Complex.re = 1.0; im = 2.0}>.);;
(*
- : Complex.t = {Complex.re = 1.; Complex.im = -2.}
*)
let x = {Complex.re = 1.0; im = 2.0} in .<x.re>.;;
(*
Characters 42-46:
  let x = {Complex.re = 1.0; im = 2.0} in .<x.re>.;;
                                            ^^^^
Error: Unbound record field label re
*)
let x = {Complex.re = 1.0; im = 2.0} in .<x.Complex.re>.;;
(*
- : ('cl, float) code =
.<((* cross-stage persistent value (as id: x) *)).Complex.re>.
*)
.!(let x = {Complex.re = 1.0; im = 2.0} in .<x.Complex.re>.);;
(* - : float = 1. *)
let x = ref 1 in .<x.contents>.;;       (* Pervasive record *)
(*
- : ('cl, int) code =
.<((* cross-stage persistent value (as id: x) *)).contents>.
*)
.!(let x = ref 1 in .<x.contents>.);;
(* - : int = 1 *)
let x = ref 1 in .<x.contents <- 2>.;;
(*
- : ('cl, unit) code =
.<((* cross-stage persistent value (as id: x) *)).contents <- 2>.
*)
let x = ref 1 in (.! .<x.contents <- 2>.); x;;
(* - : int ref = {contents = 2} *)

open Complex;;
.<{re = 1.0; im = 2.0}>.;;
(*
- : ('cl, Complex.t) code = .<{Complex.re = 1.0; Complex.im = 2.0}>.
*)
norm (.! .<{re = 1.0; im = 2.0}>.);;
(* - : float = 2.23606797749979 *)
let x = {re = 1.0; im = 2.0} in .<x.re>.;;
(*
- : ('cl, float) code =
.<((* cross-stage persistent value (as id: x) *)).Complex.re>.
*)
.!(let x = {re = 1.0; im = 2.0} in .<x.re>.);;
(* - : float = 1. *)

type foo = {fool : int};;
.<{fool = 1}>.;;
(*
Fatal error: exception Trx.TrxError("Label fool cannot be used within brackets. Put into a separate file.")
*)

(* Conditional *)

.<if true then 1 else 2>.;;
(* - : ('cl, int) code = .<if (true) then 1 else 2>. *)
<if Some 1 = None then print_string "weird">.;;
(*
- : ('cl, unit) code =
.<if ((Some (1)) = (None)) then (print_string "weird">.
*)
.! .<if Some 1 = None then print_string "weird">.;;
(* - : unit = () *)

(* Polymorphic variants *)
.<`Foo>.;;
(*
- : ('cl, [> `Foo ]) code = .<`Foo>.
*)
.<`Bar 1>.;;
(*
- : ('cl, [> `Bar of int ]) code = .<`Bar 1>.
*)
match .! .<`Bar 1>. with `Bar x -> x ;;
(* - : int = 1 *)

(* Some support for modules *)
et f = fun x -> .<x # foo>.;;
(*
val f : < foo : 'a; .. > -> ('cl, 'a) code = <fun>
*)
let x = object method foo = 1 end;;
(*
val x : < foo : int > = <obj>
*)
f x;;
(*
- : ('a, int) code =
.<(((* cross-stage persistent value (as id: x) *))#foo)>.
*)
.! (f x);;
(* - : int = 1 *)

(* Local open *)
.! .<Complex.(norm {re=1.0; im = 3.0})>.;;
(*
- : float = 3.16227766016837952
*)

