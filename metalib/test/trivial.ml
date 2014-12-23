(* Trivial tests of MetaOCaml, which are also regression tests *)
open Runcode;;

3+2;;
(* - : int = 5 *)
let rec fact = function | 0 -> 1 | n -> n * fact (n-1);;
(* val fact : int -> int = <fun> *)
let 120 = fact 5;;

.<1>.;;
(* - : int code = .<1>. *)
.<"aaa">.;;
(* - : string code = .<"aaa">. *)
let 1 = !. .<1>.;;
(* - : int = 1 *)

close_code .<1>.;;
(* - : int Runcode.closed_code = .<1>.  *)

(* Problem with special treatment of top-level identifiers by the
   type checker
*)
List.rev;;
(* - : 'a list -> 'a list = <fun> *)

(* Also check the generalization *)
.<List.rev>.;;
(* - : ('a list -> 'a list) code = .<List.rev>. *)

.<fun x -> .~(let y = x in y)>.;;
(*
Characters 22-23:
  .<fun x -> .~(let y = x in y)>.;;
                        ^
Error: Wrong level: variable bound at level 1 and used at level 0
*)
print_endline "Error was expected";;

.<fun x -> 1 + .~(.<true>.)>.;;
(*
Characters 20-24:
  .<fun x -> 1 + .~(.<true>.)>.;;
                      ^^^^
Error: This expression has type bool but an expression was expected of type
         int
*)
print_endline "Error was expected";;


(* CSP *)

let x = 1 in .<x>.;;
(*
- : int code = .<1>.
*)
let x = 1.0 in .<x>.;;
(*
- : float code = .<1.>.
*)
let x = true in .<x>.;;
(*
- : bool code = .<true>.
*)
let x = "aaa" in .<x>.;;
(*
- : string code = .<"aaa">.
*)
let x = 'a' in .<x>.;;
(*
- : char code = .<'a'>.
*)
let x = ['a'] in .<x>.;;
(*
- : char list code = .<(* CSP x *)>. 
*)

let l x = .<x>.;;                       (* polymorphic *)
(* val l : 'a -> ('cl, 'a) code = <fun> *)
l 1;;
(*
- : int code = .<(* CSP x *) Obj.magic 1>. 
*)
let 1 = !. (l 1);;
l 1.0;;                  (* better printing in N100 *)
(*
- : float code = .<1.>.
*)
let 1.0 = !. (l 1.0);;
l [];;                                  (* serializable code in N102 *)
(*
- : 'a list code = .<(* CSP x *) Obj.magic 0>. 
*)
let [] = !. (l []);;

l (fun x -> x + 1);;
(*
Characters 12-13:
  l (fun x -> x + 1);;
              ^
Warning 22: The CSP value is a closure or too deep to serialize
- : (int -> int) code = .<(* CSP x *)>. 
*)

.<List.rev>.;;
(*
- : ('a list -> 'a list) code = .<List.rev>.
*)

.<Array.get>.;;
(*
- : ('a array -> int -> 'a) code = .<Array.get>.
*)
.<(+)>.;;
(*
- : (int -> int -> int) code = .<(+)>.
*)


let x = true in .<assert x>.;;
(*
- : unit code = .<assert true>.
*)
!. .<assert (2>1)>.;;

(* Applications and labels *)
.<succ 1>.;;
(*
- : int code = .<succ 1>.
*)

let 2 = !. .<succ 1>.;;

.<1 + 2>.;;
(*
- : int code = .<(1 + 2)>.
*)
let 3 = !. .<(1 + 2)>.;;

.<String.length "abc">.;;
(*
- : int code = .<String.length "abc">.
*)
let 3 = 
  !. .<String.length "abc">.;;

.<StringLabels.sub ?pos:1 ?len:2 "abc">.;;
(*
- : string code = .<(StringLabels.sub "abc" ~pos:1 ~len:2>.
*)
let "bc" =
  !. .<StringLabels.sub ?pos:1 ?len:2 "abc">.;;

.<StringLabels.sub ~len:2 ~pos:1 "abc">.;;
(*
- : string code = .<(StringLabels.sub "abc" ~pos:1 ~len:2>.
*)
let "bc" =
  !. .<StringLabels.sub ~len:2 ~pos:1 "abc">.;;

(* Nested brackets and escapes and run *)
.<.<1>.>.;;
(* - : int code code = .<.< 1  >.>. *)
!. .<.<1>.>.;;
(* - : int code = .<1>. *)
let 1 = !. !. .<.<1>.>.;;
(* - : int = 1 *)
.<!. .<1>.>.;;
(*
- : int code = .<Runcode.( !. )  (.< 1  >.)>. 
*)
let 1 = !. .<!. .<1>.>.;;
.<1 + .~(let x = 2 in .<x>.)>.;;
(*
- : int code = .<(1 + 2)>.
*)
let x = .< 2 + 4 >. in .< .~ x + .~ x >. ;;
(*
- : int code = .<((2 + 4) + (2 + 4))>.
*)
let 12 = !. (let x = .< 2 + 4 >. in .< .~ x + .~ x >. );;

.<1 + .~(let x = 2 in .<.<x>.>.)>.;;
(*
Characters 24-29:
  .<1 + .~(let x = 2 in .<.<x>.>.)>.;;
                          ^^^^^
Error: This expression has type 'a code
       but an expression was expected of type int
*)
print_endline "Error was expected";;
.<1 + !. .~(let x = 2 in .<.<x>.>.)>.;;
(*
- : int code = .<1 + (Runcode.( !. )  (.< 2  >.))>. 
*)
let 3 = !. .<1 + !. .~(let x = 2 in .<.<x>.>.)>.;;
!. .<1 + .~ (.~(let x = 2 in .<.<x>.>.))>.;;
(*
Characters 12-40:
  !. .<1 + .~ (.~(let x = 2 in .<.<x>.>.))>.;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong level: escape at level 0
*)
print_endline "Error was expected";;

.<.<.~(.<1>.)>.>.;;
(*
- : int code code = .<.< .~.< 1  >.  >.>. 
- : ('cl, ('cl0, int) code) code = .<.<.~(.<1>.)>.>.
*)
!. .<.<.~(.<1>.)>.>.;;
(*
- : int code = .<1>.
*)
.<.<.~.~(.<.<1>.>.)>.>.;;
(*
- : int code code = .<.< .~.< 1  >.  >.>. 
- : ('cl, ('cl0, int) code) code = .<.<.~(.<1>.)>.>.
*)

(* Lazy *)
.<lazy 1>.;;
(*
- : int lazy_t code = .<lazy 1>. 
*)
let 1 = Lazy.force (!. .<lazy 1>.);;

(* Tuples *)
.<(1,"abc")>.;;
(*
- : (int * string) code = .<(1, "abc")>. 
*)
.<(1,"abc",'d')>.;;
(*
- : (int * string * char) code = .<(1, "abc", 'd')>. 
*)
let "abc" =
  match !. .<(1,"abc",'d')>. with (_,x,_) -> x;;

(* Arrays *)
.<[||]>.;;
(*
- : 'a array code = .<[||]>. 
*)
let x = .<1+2>. in .<[|.~x;.~x|]>.;;
(*
- : int array code = .<[|(1 + 2);(1 + 2)|]>. 
*)

(* Constructors and enforcing externality *)
.<raise Not_found>.;;
(*
- : 'a code = .<(raise (Not_found)>.
*)
.<raise (Scan_failure "")>.;;
(*
Characters 8-25:
  .<raise (Scan_failure "")>.;;
          ^^^^^^^^^^^^^^^^^
Error: Unbound constructor Scan_failure
*)
print_endline "Error was expected";;
.<raise (Scanf.Scan_failure "")>.;;
(*
- : 'a code = .<(raise (Scanf.Scan_failure (""))>.
*)
open Scanf;;
.<raise (Scan_failure "")>.;;
(*
- : 'a code = .<Pervasives.raise (Scanf.Scan_failure "")>. 
*)
!. .<raise (Scan_failure "")>.;;
(*
Exception: Scanf.Scan_failure "".
*)
print_endline "Exception was expected";;


.<true>.;;
(*
- : bool code = .<true>. 
*)
.<Some 1>.;;
(*
- : int option code = .<Some 1>. 
*)
.<Some [1]>.;;
(*
- : int list option code = .<Some [1]>. 
*)
let Some [1] = !. .<Some [1]>.;;
.<None>.;;
(*
- : 'a option code = .<None>. 
*)
let None = !. .<None>.;;

.<Genlex.Int 1>.;;
(*
- : Genlex.token code = .<Genlex.Int 1>. 
*)
open Genlex;;
.<Int 1>.;;
(*
- : Genlex.token code = .<Genlex.Int 1>. 
*)
let Int 1 = !. .<Int 1>.;;

module Foo = struct exception E end;;
.<raise Foo.E>.;;
(*
Fatal error: exception Trx.TrxError("Exception Foo.E cannot be used within brackets. Put into a separate file.")
*)
print_endline "Error was expected";;

type foo = Bar;;
.<Bar>.;;
(*
Characters 2-5:
  .<Bar>.;;
    ^^^
Error: Unqualified constructor Bar cannot be used within brackets. Put into a separate file.
*)
print_endline "Error was expected";;

module Foo = struct type foo = Bar end;;
.<Foo.Bar>.;;
(*
Characters 2-9:
  .<Foo.Bar>.;;
    ^^^^^^^
Error: Constructor Bar cannot be used within brackets. Put into a separate file.
*)
print_endline "Error was expected";;

(* Records *)

.<{Complex.re = 1.0; im = 2.0}>.;;
(*
- : ('cl, Complex.t) code = .<{Complex.re = 1.0; Complex.im = 2.0}>.
*)
let {Complex.re = 1.; Complex.im = -2.} =
  Complex.conj (!. .<{Complex.re = 1.0; im = 2.0}>.);;
let x = {Complex.re = 1.0; im = 2.0} in .<x.re>.;;
(*
Characters 44-46:
  let x = {Complex.re = 1.0; im = 2.0} in .<x.re>.;;
                                              ^^
Warning 40: re was selected from type Complex.t.
It is not visible in the current scope, and will not 
be selected if the type becomes unknown.
- : float code = .<((* cross-stage persistent value (id: x) *)).Complex.re>. 
*)

let x = {Complex.re = 1.0; im = 2.0} in .<x.Complex.re>.;;
(*
- : float code =
.<((* cross-stage persistent value (as id: x) *)).Complex.re>.
*)
let 1.0 = !.(let x = {Complex.re = 1.0; im = 2.0} in .<x.Complex.re>.);;
let x = ref 1 in .<x.contents>.;;       (* Pervasive record *)
(*
- : int code =
.<((* cross-stage persistent value (as id: x) *)).contents>.
*)
let 1 = !.(let x = ref 1 in .<x.contents>.);;
let x = ref 1 in .<x.contents <- 2>.;;
(*
- : unit code =
.<((* cross-stage persistent value (as id: x) *)).contents <- 2>.
*)
let x = ref 1 in (!. .<x.contents <- 2>.); x;;
(* - : int ref = {contents = 2} *)

open Complex;;
.<{re = 1.0; im = 2.0}>.;;
(*
- : Complex.t code = .<{Complex.re = 1.0; Complex.im = 2.0}>.
*)
let 5.0 = norm (!. .<{re = 3.0; im = 4.0}>.);;
let x = {re = 1.0; im = 2.0} in .<x.re>.;;
(*
- : float code =
.<((* cross-stage persistent value (as id: x) *)).Complex.re>.
*)
let 1.0 = !.(let x = {re = 1.0; im = 2.0} in .<x.re>.);;

type foo = {fool : int};;
.<{fool = 1}>.;;
(*
Characters 3-7:
  .<{fool = 1}>.;;
     ^^^^
Error: Unqualified label fool cannot be used within brackets. Put into a separate file.
*)
print_endline "Error was expected";;

(* Conditional *)

.<if true then 1 else 2>.;;
(* - : int code = .<if true then 1 else 2>. *)
.<if Some 1 = None then print_string "weird">.;;
(*
- : unit code = .<if (Some 1) = None then Pervasives.print_string "weird">. 
*)
let () = !. .<if Some 1 = None then print_string "weird">.;;

(* Polymorphic variants *)
.<`Foo>.;;
(*
- : [> `Foo ] code = .<`Foo>. 
*)
.<`Bar 1>.;;
(*
- : [> `Bar of int ] code = .<`Bar 1>. 
*)
let 1 = match !. .<`Bar 1>. with `Bar x -> x ;;

(* Some support for modules *)
let f = fun x -> .<x # foo>.;;
(*
val f : < foo : 'a; .. > -> 'a code = <fun>
*)
let x = object method foo = 1 end;;
(*
val x : < foo : int > = <obj>
*)
f x;;
(*
- : int code = .<((* cross-stage persistent value (id: x) *))#foo>. 
*)
let 1 = !. (f x);;

(* Local open *)
let 5.0 = !. .<Complex.(norm {re=3.0; im = 4.0})>.;;

let 5.0 = !. .<let open Complex in norm {re=4.0; im = 3.0}>.;;

(* For-loop *)

.<for i=1 to 5 do Printf.printf "ok %d %d\n" i (i+1) done>.;;
(*
- : unit code = .<
for i_1 = 1 to 5 do Printf.printf "ok %d %d\n" i_1 (i_1 + 1) done>. 
*)
!. .<for i=1 to 5 do Printf.printf "ok %d %d\n" i (i+1) done>.;;
(*
ok 1 2
ok 2 3
ok 3 4
ok 4 5
ok 5 6
*)

.<for i=5 downto 1 do Printf.printf "ok %d %d\n" i (i+1) done>.;;
(*
- : unit code = .<
for i_3 = 5 downto 1 do Printf.printf "ok %d %d\n" i_3 (i_3 + 1) done>. 
*)
!. .<for i=5 downto 1 do Printf.printf "ok %d %d\n" i (i+1) done>.;;
(*
ok 5 6
ok 4 5
ok 3 4
ok 2 3
ok 1 2
*)

.<for i=1 to 2 do for j=1 to 3 do Printf.printf "ok %d %d\n" i j done done>.;;
(*
- : unit code = .<
for i_5 = 1 to 2 do
  for j_6 = 1 to 3 do Printf.printf "ok %d %d\n" i_5 j_6 done
done>. 
*)
!. .<for i=1 to 2 do 
     for j=1 to 3 do Printf.printf "ok %d %d\n" i j done done>.;;
(*
ok 1 1
ok 1 2
ok 1 3
ok 2 1
ok 2 2
ok 2 3
*)

let c = .<for i=1 to 2 do .~(let x = .<i>. in 
             .<for i=1 to 3 do Printf.printf "ok %d %d\n" i .~x done>.) done>.;;
(*
val c : unit code = .<
  for i_9 = 1 to 2 do
    for i_10 = 1 to 3 do Printf.printf "ok %d %d\n" i_10 i_9 done
  done>. 
*)
!. c;;
(*
ok 1 1
ok 2 1
ok 3 1
ok 1 2
ok 2 2
ok 3 2
*)

(* Scope extrusion test *)

.<fun x -> .~(!. .<x>.; .<1>.)>.;;
(*
Exception:
Failure
 "The code built at Characters 6-7:\n  .<fun x -> .~(!. .<x>.; .<1>.)>.;;\n        ^\n is not closed: identifier x_28 bound at Characters 6-7:\n  .<fun x -> .~(!. .<x>.; .<1>.)>.;;\n        ^\n is free".

Was:
Characters 14-22:
  .<fun x -> .~(!. .<x>.; .<1>.)>.;;
                ^^^^^^^^
Error: !. error: 'cl not generalizable in ('cl, 'a) code
*)
print_endline "Error was expected";;

let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<0>.; .<()>.) done>.; 
                     .<for i=1 to 5 do ignore (.~(!r)) done>.;;
(*
- : unit code = .<for i_13 = 1 to 5 do Pervasives.ignore 0 done>. 
*)

let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<i>.; .<()>.) done>.; 
                     .<for i=1 to 5 do ignore (.~(!r)) done>.;;
(*
Exception:
Failure
 "Scope extrusion detected at Characters 110-125:\n                       .<for i=1 to 5 do ignore (.~(!r)) done>.;;\n                                         ^^^^^^^^^^^^^^^\n for code built at Characters 27-28:\n  let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<i>.; .<()>.) done>.; \n                             ^\n for the identifier i_14 bound at Characters 27-28:\n  let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<i>.; .<()>.) done>.; \n                             ^\n".
*)
print_endline "Error was expected";;

(* Better error message *)
let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<i+1>.; .<()>.) done>.; 
                     .<for i=1 to 5 do ignore (.~(!r)) done>.;;

(*
Characters 21-70:
  let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<i+1>.; .<()>.) done>.; 
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 10: this expression should have type unit.
Exception:
Failure
 "Scope extrusion detected at Characters 112-127:\n                       .<for i=1 to 5 do ignore (.~(!r)) done>.;;\n                                         ^^^^^^^^^^^^^^^\n for code built at Characters 49-52:\n  let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<i+1>.; .<()>.) done>.; \n                                                   ^^^\n for the identifier i_16 bound at Characters 27-28:\n  let r = ref .<0>. in .<for i=1 to 5 do .~(r := .<i+1>.; .<()>.) done>.; \n                             ^\n".
*)
print_endline "Error was expected";;

(* Simple functions *)
.<fun x -> x>.;;
(*
- : ('a -> 'a) code = .<fun x_7  -> x_7>. 
*)
let 42 = (!. .<fun x -> x>.) 42;;

.<fun x y -> x + y>.;;
(*
- : (int -> int -> int) code = .<fun x_9  y_10  -> x_9 + y_10>. 
*)
let 5 = (!. .<fun x y -> x + y>.) 2 3;;

.<fun x -> fun x -> x + x >.;;
(*
- : ('a -> int -> int) code = .<fun x_13  x_14  -> x_14 + x_14>. 
*)
let 8 = !. .<fun x -> fun x -> x + x >. 3 4;;

(* Testing hygiene  *)
let eta f = .<fun x -> .~(f .<x>.)>.;;
(*
val eta : ('a code -> 'b code) -> ('a -> 'b) code = <fun>
*)
eta (fun x -> .<1 + .~x>.);;
(*
- : (int -> int) code = .<fun x_17  -> 1 + x_17>. 
*)
eta (fun y -> .<fun x -> x + .~y>.);;
(*
- : (int -> int -> int) code = .<fun x_18  x_19  -> x_19 + x_18>. 
*)
let 5 = (!. (eta (fun y -> .<fun x -> x + .~y>.))) 2 3;;

(* new identifiers must be generated at run-time *)
let rec fhyg = fun y -> function
  | 0 -> y
  | n -> .<(fun x -> .~(fhyg .<.~y + x>. (n-1))) n>.;;
(*
val fhyg : int -> int code = <fun>
*)
fhyg .<1>. 3;;
(*
- : int code = .<
(fun x_7  -> (fun x_8  -> (fun x_9  -> ((1 + x_7) + x_8) + x_9) 1) 2) 3>. 
*)
let 7 = !. (fhyg .<1>. 3);;

(* pattern-matching, general functions *)

.<fun () -> 1>.;;
(* - : ('cl, unit -> int) code = .<fun () -> 1>. *)
!. .<fun () -> 1>.;;
(* - : unit -> int = <fun> *)
let 1 = (!. .<fun () -> 1>.) ();;

.<function true -> 1 | false -> 0>.;;
(*
- : ('cl, bool -> int) code = .<function | true -> 1 | false -> 0>. 
*)
let 1 = (!. .<function true -> 1 | false -> 0>.) true;;

.<fun (true,[]) -> 1>.;;
(*
- : ('cl, bool * 'a list -> int) code = .<fun (true, []) -> 1>. 
*)
(!. .<fun (true,[]) -> 1>.) (true,[1]);;
(*
Exception: Match_failure ("//toplevel//", 1, 6).
*)
print_endline "Error was expected";;
let 1 = (!. .<fun (true,[]) -> 1>.) (true,[]);;

.<fun [|true;false;false|] -> 1>.;;
(*
- : ('cl, bool array -> int) code = .<fun [|true; false; false|] -> 1>. 
*)
let 1 = (!. .<fun [|true;false;false|] -> 1>.) [|true;false;false|];;

.<function `F 1 -> true | _ -> false>.;;
(*
- : ('cl, [> `F of int ] -> bool) code = .<
function | (`F 1) -> true | _ -> false>. 
*)
let true = (!. .<function `F 1 -> true | _ -> false>.) (`F 1);;
.<function `F 1 | `G 2 -> true | _ -> false>.;;
(*
- : ('cl, [> `F of int | `G of int ] -> bool) code = .<
function | ((`F 1) | (`G 2)) -> true | _ -> false>. 
*)

.<function (1,"str") -> 1 | (2,_) -> 2>.;;
(*
- : ('cl, int * string -> int) code = .<
function | (1, "str") -> 1 | (2, _) -> 2>. 
*)
let 1 = (!. .<function (1,"str") -> 1 | (2,_) -> 2>.) (1,"str");;
let 2 = (!. .<function (1,"str") -> 1 | (2,_) -> 2>.) (2,"str");;
let 1 = (!. .<fun [1;2] -> 1>.) [1;2];;

let 2 = (!. .<function None -> 1 | Some [1] -> 2>.) (Some [1]);;

let 2 = (!. .<function (Some (Some true)) -> 1 | _ -> 2>.) (Some None);;
let 1 = (!. .<function (Some (Some true)) -> 1 | _ -> 2>.) (Some (Some true));;
let 2 = (!. .<function (Some (Some true)) -> 1 | _ -> 2>.) (Some (Some false));;
let 2 = (!. .<function (Some (Some true)) -> 1 | _ -> 2>.) None;;

open Complex;;
.<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.;;
(*
- : ('cl, Complex.t -> int) code = .<
function
| {Complex.re = 1.0} -> 1
| {Complex.re = 2.0; Complex.im = 2.0} -> 2
| {Complex.im = _} -> 3>. 
*)

let 1 = (!. .<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.)
        {re=1.0; im=2.0};;
let 2 = (!. .<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.)
        {re=2.0; im=2.0};;
(* - : int = 2 *)
let 3 = (!. .<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.)
        {re=2.0; im=3.0};;

(* General functions *)

(* Non-binding pattern *)
.<fun () -> 1>.;;
(*
- : (unit -> int) code = .<fun ()  -> 1>. 
*)
let 1 = !. .<fun () -> 1>. ();;

.<fun _ -> true>.;;
(* - : ('a -> bool) code = .<fun _  -> true>. *)
let true = !. .<fun _ -> true>. 1;;

.<fun (x,y) -> x + y>.;;
(*
- : (int * int -> int) code = .<fun (x_1,y_2)  -> x_1 + y_2>. 
*)
let 5 = (!. .<fun (x,y) -> x + y>.) (2,3);;
.<function (Some x) as y -> x | _ ->  2>.;;
(*
- : (int option -> int) code = .<function | Some x_5 as y_6 -> x_5 | _ -> 2>. 
*)
let 1 = (!. .<function (Some x) as y -> x | _ ->  2>.) (Some 1);;
let 2 = (!. .<function (Some x) as y -> x | _ ->  2>.) None;;
.<function [x;y;z] -> x - y + z | [x;y] -> x - y>.;;
(*
- : (int list -> int) code = .<
function
| x_11::y_12::z_13::[] -> (x_11 - y_12) + z_13
| x_14::y_15::[] -> x_14 - y_15>. 
*)
let 2 = (!. .<function [x;y;z] -> x - y + z | [x;y] -> x - y>.) [1;2;3];;

 (* OR patterns *)
.<function ([x;y] | [x;y;_]) -> x - y>.;;
(*
- : (int list -> int) code = .<
fun (x_21::y_22::[]|x_21::y_22::_::[])  -> x_21 - y_22>. 
*)
let -1 = (!. .<function ([x;y] | [x;y;_]) -> x - y>.) [1;2];;
let -1 = (!. .<function ([x;y] | [x;y;_]) -> x - y>.) [1;2;3];;
(!. .<function ([x;y] | [x;y;_]) -> x - y>.) [1;2;3;4];;
(* Exception: Match_failure ("//toplevel//", 1, 6). *)
print_endline "Error was expected";;

.<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.;;
(*
- : (int list -> int) code = .<
fun (x_29::y_30::[]|x_29::y_30::_::[]|y_30::x_29::_::_::[])  -> x_29 - y_30>. 
*)
let -1 = (!. .<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.) [1;2];;
let -1 = (!. .<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.) [1;2;3];;
let  1 = (!. .<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.) [1;2;3;4];;

.<function (`F x | `G x) -> x | `E x -> x>.;;
(*
- : ([< `E of 'a | `F of 'a | `G of 'a ] -> 'a) code = .<
function | `F x_37|`G x_37 -> x_37 | `E x_38 -> x_38>. 
*)
let 2 = (!. .<function (`F x | `G x) -> x | `E x -> x>.) (`F 2);;
open Complex;;
.<function {re=x} -> x | {im=x; re=y} -> x -. y>.;;
(*
- : (Complex.t -> float) code = .<
function
| { Complex.re = x_41 } -> x_41
| { Complex.re = y_42; Complex.im = x_43 } -> x_43 -. y_42>. 
*)
.<function {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.;;
(*
- : (Complex.t -> float) code = .<
function
| { Complex.re = x_44; Complex.im = 2.0 } -> x_44
| { Complex.re = y_45; Complex.im = x_46 } -> x_46 -. y_45>. 
*)
let 0. = (!. .<function {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.) 
         {re=1.0; im=1.0};;
let 1. = (!. .<function {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.) 
         {re=1.0; im=2.0};;
.<function (Some x) as y when x  > 0 -> y | _ -> None>.;;
(*
- : (int option -> int option) code = .<
function | Some x_53 as y_54 when x_53 > 0 -> y_54 | _ -> None>. 
*)
let Some 1 = (!. .<function (Some x) as y when x  > 0 -> y | _ -> None>.)
             (Some 1);;
let None = (!. .<function (Some x) as y when x  > 0 -> y | _ -> None>.)
           (Some 0);;

(* pattern-matching *)
.<match 1 with 1 -> true>.;;
(*
- : bool code = .<match 1 with | 1 -> true>. 
*)
let true = !. .<match 1 with 1 -> true>.;;

.<match (1,2) with (1,x) -> true | x -> false>.;;
(*
- : bool code = .<match (1, 2) with | (1,x_5) -> true | x_6 -> false>. 
*)
.<match [1;2] with [x] -> x | [x;y] -> x + y>.;;
(*
- : int code = .<
match [1; 2] with | x_7::[] -> x_7 | x_8::y_9::[] -> x_8 + y_9>. 
*)
let 3 = 
  !. .<match [1;2] with [x] -> x | [x;y] -> x + y>.;;

(* OR patterns *)
.<match [1;2] with [x] -> x | [x;y] | [x;y;_] -> x + y>.;;
(*
- : int code = .<
match [1; 2] with
| x_13::[] -> x_13
| x_14::y_15::[]|x_14::y_15::_::[] -> x_14 + y_15>. 
*)
let 3 = !. .<match [1;2] with [x] -> x | [x;y] | [x;y;_] -> x + y>.;;

.<match [1;2;3;4] with [x] -> x | [x;y] | [x;y;_] | [y;x;_;_] -> x - y>.;;
(*
- : int code = .<
match [1; 2; 3; 4] with
| x_19::[] -> x_19
| x_20::y_21::[]|x_20::y_21::_::[]|y_21::x_20::_::_::[] -> x_20 - y_21>. 
*)
let 1 =
  !. .<match [1;2;3;4] with [x] -> x | [x;y] | [x;y;_] | [y;x;_;_] -> x - y>.;;

.<fun x -> match x with (`F x | `G x) -> x | `E x -> x>.;;
(*
- : ([< `E of 'a | `F of 'a | `G of 'a ] -> 'a) code = .<
fun x_25  -> match x_25 with | `F x_26|`G x_26 -> x_26 | `E x_27 -> x_27>. 
*)

let 1 = (!. .<fun x -> match x with (`F x | `G x) -> x | `E x -> x>.) (`G 1);;

open Complex;;
.<fun x -> match x with {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.;;
(*
- : (Complex.t -> float) code = .<
fun x_31  ->
  match x_31 with
  | { Complex.re = x_32; Complex.im = 2.0 } -> x_32
  | { Complex.re = y_33; Complex.im = x_34 } -> x_34 -. y_33>.
*)

let 1.0 =
  (!. .<fun x -> match x with {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.)
    {im=2.0; re=1.0};;


(* try *)
.<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.;;
(*
- : (int -> bool option) code = .<
fun x_39  ->
  try Some (List.assoc x_39 [(1, true); (2, false)])
  with | Not_found  -> None>.
*)
let Some true =
  (!. .<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.) 1;;
let Some false =
 (!. .<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.) 2;;
let None =
 (!. .<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.) 3;;

.<fun x -> let open Scanf in try sscanf x "%d" (fun x -> string_of_int x) with Scan_failure x -> "fail " ^ x>.;;
(*
- : (string -> string) code = .<
fun x_43  ->
  let open Scanf in
    try Scanf.sscanf x_43 "%d" (fun x_44  -> Pervasives.string_of_int x_44)
    with | Scanf.Scan_failure x_45 -> "fail " ^ x_45>.
*)

let "1" = 
  (!. .<fun x -> let open Scanf in try sscanf x "%d" (fun x -> string_of_int x) with Scan_failure x -> "fail " ^ x>.) "1";;
let "fail scanf: bad input at char number 0: 'character 'x' is not a decimal digit'" =
 (!. .<fun x -> let open Scanf in try sscanf x "%d" (fun x -> string_of_int x) with Scan_failure x -> "fail " ^ x>.) "xxx";;

(* Simple let *)

.<let x = 1 in x>.;;
(*
- : int code = .<let x_1 = 1 in x_1>. 
*)
let 1 = 
  !. .<let x = 1 in x>.;;
.<let x = 1 in let x = x + 1 in x>.;;
(*
- : int code = .<let x_55 = 1 in let x_56 = x_55 + 1 in x_56>. 
*)
let 2 = 
  !. .<let x = 1 in let x = x + 1 in x>.;;
.<let rec f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;
(*
- : int code = .<
let rec f_7 n_8 = if n_8 = 0 then 1 else n_8 * (f_7 (n_8 - 1)) in f_7 5>. 
*)
let 120 =
  !. .<let rec f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;

(* Recursive vs. non-recursive bindings *)
.<let f = fun x -> x in 
  let rec f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;
(*
  Characters 6-7:
  .<let f = fun x -> x in 
        ^
Warning 26: unused variable f.
- : int code = .<
let f_12 x_11 = x_11 in
let rec f_13 n_14 = if n_14 = 0 then 1 else n_14 * (f_13 (n_14 - 1)) in
f_13 5>. 
*)

let 120 = !. .<let f = fun x -> x in 
               let rec f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;

.<let f = fun x -> x in 
  let f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;
(*
  - : int code = .<
let f_20 x_19 = x_19 in
let f_22 n_21 = if n_21 = 0 then 1 else n_21 * (f_20 (n_21 - 1)) in f_22 5>. 
*)
let 20 = !. .<let f = fun x -> x in 
              let f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;

.<let rec [] = [] in []>.
(*
Characters 2-23:
  .<let rec [] = [] in []>.;;
    ^^^^^^^^^^^^^^^^^^^^^
Error: Only variables are allowed as left-hand side of `let rec'
*)
print_endline "Error was expected";;

.<let rec f = f in f>.
(*
Exception:
Failure
 "Recursive let binding Characters 2-20:\n  .<let rec f = f in f>.;;\n    ^^^^^^^^^^^^^^^^^^\n must be to a function Characters 10-11:\n  .<let rec f = f in f>.;;\n            ^\n".
*)
print_endline "Error was expected";;

(* General let rec *)
.<fun x -> let rec even = function 0 -> true | x -> odd (x-1) and 
                   odd  = function 0 -> false | x -> even (x-1) in even x>.;;
(*
  - : (int -> bool) code = .<
fun x_80  ->
  let rec even_81 = function | 0 -> true | x_84 -> odd_82 (x_84 - 1)
  and odd_82 = function | 0 -> false | x_83 -> even_81 (x_83 - 1) in
  even_81 x_80>.
*)
let true = (!. .<fun x -> let rec even = function 0 -> true | x -> odd (x-1) and odd = function 0 -> false | x -> even (x-1) in even x>.) 42;;
let false = (!. .<fun x -> let rec even = function 0 -> true | x -> odd (x-1) and odd = function 0 -> false | x -> even (x-1) in even x>.) 43;;


(* General let *)
.<let x = 1 and y = 2 in x + y>.;;
(*
- : int code = .<let x_73 = 1 and y_74 = 2 in x_73 + y_74>. 
*)
let 3 = !. .<let x = 1 and y = 2 in x + y>.;;

.<let x = 1 in let x = x+1 and y = x+1 in x + y>.;;
(*
- : int code = .<
let x_77 = 1 in let x_78 = x_77 + 1 and y_79 = x_77 + 1 in x_78 + y_79>. 
*)
let 4 = !. .<let x = 1 in let x = x+1 and y = x+1 in x + y>.;;
.<fun x -> let (Some x) = x in x + 1>.;;
(*
Characters 15-23:
  .<fun x -> let (Some x) = x in x + 1>.;;
                 ^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
None
- : (int option -> int) code = .<
fun x_83  -> let Some x_84 = x_83 in x_84 + 1>. 
*)
let 3 = (!. .<fun x -> let (Some x) = x in x + 1>.) (Some 2);;
(!. .<fun x -> let (Some x) = x in x + 1>.) None;;
(*
Characters 19-27:
  (!. .<fun x -> let (Some x) = x in x + 1>.) None;;
                     ^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
None
Exception: Match_failure ("//toplevel//", 1, 19).
*)
print_endline "Error was expected";;



(* testing scope extrusion *)
let r = ref .<0>. in let _ = .<fun x -> .~(r := .<1>.; .<0>.)>. in !r ;;
(* - : int code = .<1>.  *)
let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in !r ;;
(*
- : int code = .<x_30>.

Failure("The code built at Characters 35-36:\n  let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in !r ;;\n                                     ^\n is not closed: identifier x_30 bound at Characters 35-36:\n  let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in !r ;;\n                                     ^\n is free")
*)
print_endline "Error was expected";;

let c = let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in (!r) in !. c;;
(*
Exception:
Failure
 "The code built at Characters 43-44:\n  let c = let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in (!r) in !. c;;\n                                             ^\n is not closed: identifier x_31 bound at Characters 43-44:\n  let c = let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in (!r) in !. c;;\n                                             ^\n is free".
*)
print_endline "Error was expected";;

let r = ref .<fun y->y>. in let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in !r ;;
(*
- : ('_a -> '_a) code = .<fun y_34  -> x_33>.

Failure("The code built at Characters 57-67:\n  let r = ref .<fun y->y>. in let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in !r ;;\n                                                           ^^^^^^^^^^\n is not closed: identifier x_33 bound at Characters 42-43:\n  let r = ref .<fun y->y>. in let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in !r ;;\n                                            ^\n is free")

*)
print_endline "Error was expected";;

(* Error message is reported on splice *)
let r = ref .<fun y->y>. in 
let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in .<fun x -> .~(!r) 1>. ;;
(*
Exception:
Failure
 "Scope extrusion detected at Characters 95-103:\n  let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in .<fun x -> .~(!r) 1>. ;;\n                                                                    ^^^^^^^^\n for code built at Characters 58-68:\n  let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in .<fun x -> .~(!r) 1>. ;;\n                               ^^^^^^^^^^\n for the identifier x_36 bound at Characters 43-44:\n  let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in .<fun x -> .~(!r) 1>. ;;\n                ^\n".
*)
print_endline "Error was expected";;

(* Unlike BER N100, the test is exact. The following is accepted with BER N101
   (it wasn't with BER N100) 
*)
let r = ref .<fun y->y>. in let _ = .<fun x -> .~(r := .<fun y -> y>.; .<0>.)>. in !r ;;
(*
- : ('_a -> '_a) code = .<fun y_41  -> y_41>. 
*)
(* Was in N100: print_endline "Error was expected";; *)

(* The following are OK though *)
let r = ref .<fun y->y>. in .<fun x -> .~(r := .<fun y -> y>.; !r)>.;;
(*
- : ('_a -> '_b -> '_b) code = .<fun x_43  y_44  -> y_44>. 
*)
let r = ref .<fun y->y>. in .<fun x -> .~(r := .<fun y -> x>.; !r)>.;;
(*
- : ('_a -> '_a -> '_a) code = .<fun x_46  y_47  -> x_46>. 
*)
let 3 = 
 let r = ref .<fun y->y>. in !. .<fun x -> .~(r := .<fun y -> x>.; !r)>. 3 4;;

print_endline "\nAll done\n";;
