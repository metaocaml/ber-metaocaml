(* Trivial tests of MetaOCaml, which are also regression tests *)
open Runcode
[@@@warning "-8"]
;;

3+2;;
(* - : int = 5 *)
let rec fact = function | 0 -> 1 | n -> n * fact (n-1)
(* val fact : int -> int = <fun> *)
let 120 = fact 5

(* Before the first bracket, check how >. is handled *)
let 45 = 
  let (>.) = fun x y -> x - y and (>.>) = fun x y -> x * y
  and (>.>.) = fun x y -> 2*x + y in
  ((3 >. 1) >.>. 1) >.> (13 >. 4)
;;

.<1>.;;
(* - : int code = .<1>. *)
.<"aaa">.;;
(* - : string code = .<"aaa">. *)
let 1 = run .<1>.;;
(* - : int = 1 *)

close_code .<1>.;;
(* - : int closed_code = .<1>.  *)


(* Problem with special treatment of top-level identifiers by the
   type checker
*)
List.rev;;
(* - : 'a list -> 'a list = <fun> *)

(* Also check the generalization *)
.<List.rev>.;;
(* - : ('a list -> 'a list) code = .<Stdlib.List.rev>. *)

.<fun x -> .~(let y = x in y)>.;;
(*
  .<fun x -> .~(let y = x in y)>.;;
                        ^
Error: A variable that was bound within brackets is used outside brackets
for example: .<fun x -> .~(foo x)>.
Hint: enclose the variable in brackets,
as in: .<fun x -> .~(foo .<x>.)>.;;
*)
print_endline "Error was expected";;

.<fun x -> .~(succ x)>.;;   (* From the error message *)
print_endline "Error was expected";;

.~(.<1>.);;
(*
  .~(.<1>.);;
    ^^^^^^^
Error: An escape may appear only within brackets
*)
print_endline "Error was expected";;


.<fun x -> 1 + .~(.<true>.)>.;;
(*
Characters 17-27:
  .<fun x -> 1 + .~(.<true>.)>.;;
                   ^^^^^^^^^^
Error: This expression has type bool but an expression was expected of type
         int
*)
print_endline "Error was expected";;


(* explicit extension *)
let t = [%metaocaml.bracket 1 + 2];;
(*
val t : int code = .<1 + 2>. 
*)
let 3 = run t;;

let t = [%metaocaml.bracket [%metaocaml.bracket 1 + 2]];;
(*
val t : int code code = .<.< 1 + 2  >.>. 
*)
let 3 = run @@ run t;;

let t = [%metaocaml.bracket [%metaocaml.bracket 
            [%metaocaml.escape [%metaocaml.bracket 1 + 2]]]]
(*
val t : int code code = .<.< .~(.< 1 + 2  >.)  >.>. 
*)
let 3 = run @@ run t;;

(* CSP *)

let x = 1 in .<x>.;;
(*
- : int code = .<1>.
*)
let x = 1.0 in .<x>.;;
(*
- : float code = .<1.>.
*)
(* precision in printing FP values *)
let x = 2. /. 3. in string_of_float x;;
(*
  - : string = "0.666666666667"
*)
let x = 2. /. 3. in .<x>.;;
(*
  - : float code = .<0.66666666666666663>. 
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
The code built at File "_none_", line 1 has hardly serializable CSPs: x at Line 1, characters 19-20

It may be allowed in the future: do tell if you need it.
- : char list code = .<csp_x_1>. 
*)
let x = () in .<x>.;;
(*
- : unit code = .<()>. 
*)

let l x = .<x>.;;                       (* polymorphic *)
(* val l : 'a -> 'a code = <fun> *)
l 1;;
(*
- : int code = .<Obj.magic 1>. 
*)
let 1 = run (l 1);;
l 1.0;;
(*
- : float code = .<Obj.magic 1.>. 
*)
let 1.0 = run (l 1.0);;
l [];;                                  (* serializable code in N102 *)
(*
- : 'a list code = .<Obj.magic 0>. 
*)
let [] = run (l []);;

l (fun x -> x + 1);;
(*
The code built at File "_none_", line 1 has hardly serializable CSPs: x at Line 1, characters 12-13

It may be allowed in the future: do tell if you need it.
- : (int -> int) code = .<csp_x_2>. 

*)

.<List.rev>.;;
(*
- : ('a list -> 'a list) code = .<Stdlib.List.rev>.
*)

.<Array.get>.;;
(*
- : ('a array -> int -> 'a) code = .<Stdlib.Array.get>.
*)
.<(+)>.;;
(*
- : (int -> int -> int) code = .<Stdlib.(+)>. 
*)


let x = true in .<assert x>.;;
(*
- : unit code = .<assert true>.
*)
run .<assert (2>1)>.;;

(* Applications and labels *)
.<succ 1>.;;
(*
- : int code = .<Stdlib.succ 1>. 
*)

let 2 = run .<succ 1>.;;

.<1 + 2>.;;
(*
- : int code = .<(1 + 2)>.
*)
let 3 = run .<(1 + 2)>.;;

.<String.length "abc">.;;
(*
- : int code = .<String.length "abc">.
*)
let 3 = 
  run .<String.length "abc">.;;

.<StringLabels.sub ~pos:1 ~len:2 "abc">.;;
(*
- : string code = .<(StringLabels.sub "abc" ~pos:1 ~len:2>.
*)
let "bc" =
  run .<StringLabels.sub ~pos:1 ~len:2 "abc">.;;

.<StringLabels.sub ~len:2 ~pos:1 "abc">.;;
(*
- : string code = .<(StringLabels.sub "abc" ~pos:1 ~len:2>.
*)
let "bc" =
  run .<StringLabels.sub ~len:2 ~pos:1 "abc">.;;

(* Nested brackets and escapes and run *)
.<.<1>.>.;;
(* - : int code code = .<.< 1  >.>. *)
run .<.<1>.>.;;
(* - : int code = .<1>. *)
let 1 = run @@ run .<.<1>.>.;;
.<run .<1>.>.;;
(*
- : int code = .<Runcode.run  (.< 1  >.)>. 
*)
let 1 = run .<run .<1>.>.;;
.<1 + .~(let x = 2 in .<x>.)>.;;
(*
- : int code = .<1 + 2>.
*)
let x = .< 2 + 4 >. in .< .~ x + .~ x >. ;;
(*
- : int code = .<(2 + 4) + (2 + 4)>. 
*)
let 12 = run (let x = .< 2 + 4 >. in .< .~ x + .~ x >. );;

.<1 + .~(let x = 2 in .<.<x>.>.)>.;;
(*
  .<1 + .~(let x = 2 in .<.<x>.>.)>.;;
                            ^
Error: This expression has type 'a code
       but an expression was expected of type int
*)
print_endline "Error was expected";;
.<1 + run .~(let x = 2 in .<.<x>.>.)>.;;
(*
- : int code = .<1 + (Runcode.run .< 2  >.)>. 
*)
let 3 = run .<1 + run .~(let x = 2 in .<.<x>.>.)>.;;
run .<1 + .~ (.~(let x = 2 in .<.<x>.>.))>.;;
(*
Line 1, characters 10-41:
1 | run .<1 + .~ (.~(let x = 2 in .<.<x>.>.))>.;;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An escape may appear only within brackets
*)
print_endline "Error was expected";;

.<.<.~(.<1>.)>.>.;;
(*
- : int code code = .<.< .~(.< 1  >.)  >.>. 
- : ('cl, ('cl0, int) code) code = .<.<.~(.<1>.)>.>.
*)
run .<.<.~(.<1>.)>.>.;;
(*
- : int code = .<1>.
*)
let 1 = run @@ run .<.<.~(.<1>.)>.>.;;

.<.<.~(.~(.<.<1>.>.))>.>.;;
(*
- : int code code = .<.< .~(.< 1  >.)  >.>. 
- : ('cl, ('cl0, int) code) code = .<.<.~(.<1>.)>.>.
*)
let 1 = run @@ run .<.<.~(.~(.<.<1>.>.))>.>.;;

(* Nested brackets and escapes on the same identifier *)
let x = .<1>. in .<.~x>.;;
(*
- : int code = .<1>. 
*)
let 1 = run (let x = .<1>. in .<.~x>.);;

let x = .<1>. in .<.~(.<x>.)>.;;
(*
The code built at File "_none_", line 1 has hardly serializable CSPs: x at Line 1, characters 24-25

It may be allowed in the future: do tell if you need it.
- : int code code = .<csp_x_3>. 
*)
run (let x = .<1>. in .<.~(.<x>.)>.);;
(*
- : int code = .<1>. 
*)
let 1 = run @@ run (let x = .<1>. in .<.~(.<x>.)>.);;

let x = .<1>. in .<.<.~x>.>.;;
(*
The code built at Line 1, characters 21-24 has hardly serializable CSPs: x at Line 1, characters 23-24

It may be allowed in the future: do tell if you need it.
- : int code code = .<.< .~csp_x_6  >.>. 
*)
let 1 = run @@ run (let x = .<1>. in .<.<.~x>.>.);;

.<.<.~(.<List.rev>.)>.>.;;
(*
- : ('a list -> 'a list) code code = .<.< .~(.< Stdlib.List.rev  >.)  >.>. 
*)
run .<.<.~(.<List.rev>.)>.>.;;
(*
- : ('_weak2 list -> '_weak2 list) code = .<Stdlib.List.rev>. 
*)
let [3;2;1] = (run @@ run .<.<.~(.<List.rev>.)>.>.) [1;2;3];;

(* from the translation paper. << ~(<1>) >> was already covered *)
let t = .<fun x -> .<x+1>.>.
(*
  val t : (int -> int code) code = .<fun x_307 -> .< x_307 + 1  >.>. 
*)
let _ = run t 4
(*
      - : int code = .<4 + 1>. 
*)
let 5 = run (run t 4)

let t = let x = 1 in .< .< x >. >.
(*
        val t : int code code = .<.< 1  >.>. 
*)
let 1 = run (run t)
let t = .<fun x -> .<fun y -> x + y>.>.
(*
    - : (int -> (int -> int) code) code = .<
fun x_308 -> .< fun y_309 -> x_308 + y_309  >.>. 
*)
let _ = run t 4
(*
  - : (int -> int) code = .<fun y_311_312 -> 4 + y_311_312>. 
*)
let 9 = run (run t 4) 5

let t = let f x = .<1 + .~x>. in .<.<fun x -> .~(f x)>.>.;;
(*
  Line 1, characters 51-52:
1 | let t = let f x = .<1 + .~x>. in .<.<fun x -> .~(f x)>.>.
                                                       ^
Error: Wrong level: variable bound at level 2 and used at level 1
*)
print_endline "Error was expected";;
let t = let f x = .<1 + .~x>. in .<.<fun x -> .~(f .<x>.)>.>.
(*
  The code built at Line 1, characters 35-59 has hardly serializable CSPs: f at Line 1, characters 49-50

It may be allowed in the future: do tell if you need it.
val t : (int -> int) code code = .<
  .< fun x_314 -> .~(csp_f_315 (.< x_314  >.))  >.>. 
*)
let t = run t
(*
  Line 1, characters 51-56:
 | .............
 | ..
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.
- : (int -> int) code = .<fun x_314_316 -> 1 + x_314_316>. 
*)
let 5 = run t 4;;

(* we use a sequence internally to represent escapes
   in a Typedtree
*)
.<.<begin assert true; 1 end>.>.;;
(*
- : int code code = .<.< assert true; 1  >.>. 
*)
let 1 = run @@ run .<.<begin assert true; 1 end>.>.;;

.<.~(.<begin assert true; 1 end>.)>.;;
(*
- : int code = .<assert true; 1>. 
*)
let x = .<begin assert true; 1 end>. in .<.~x>.;;
(*
- : int code = .<assert true; 1>. 
*)
let x = .<begin assert true; 1 end>. in .<.~(assert true; x)>.;;
(*
- : int code = .<assert true; 1>. 
*)
let 1 =
  let x = .<begin assert true; 1 end>. in run .<.~(assert true; x)>.;;

let x = .<begin assert true; 1 end>. in .<.~(ignore(run x); x)>.;;
(*
- : int code = .<assert true; 1>. 
*)
;;

(* Lazy *)
.<lazy 1>.;;
(*
- : int lazy_t code = .<lazy 1>. 
*)
let 1 = Lazy.force (run .<lazy 1>.);;

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
  match run .<(1,"abc",'d')>. with (_,x,_) -> x;;

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
- : 'a code = .<Stdlib.raise Not_found>.
*)
.<raise (Scan_failure "")>.;;
(*
Characters 9-21:
  .<raise (Scan_failure "")>.;;
           ^^^^^^^^^^^^
Error: This variant expression is expected to have type exn
       The constructor Scan_failure does not belong to type exn
*)
print_endline "Error was expected";;
.<raise (Scanf.Scan_failure "")>.;;
(*
- : 'a code = .<Stdlib.raise (Stdlib.Scanf.Scan_failure "")>. 
*)
open Scanf;;
.<raise (Scan_failure "")>.;;
(*
- : 'a code = .<Stdlib.raise (Stdlib.Scanf.Scan_failure "")>. 
*)
run .<raise (Scan_failure "")>.;;
(*
Exception: Stdlib.Scanf.Scan_failure "".
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
let Some [1] = run .<Some [1]>.;;
.<None>.;;
(*
- : 'a option code = .<None>. 
*)
let None = run .<None>.;;

.<Either.Left 1>.;;
(*
- : (int, 'a) Either.t code = .<Stdlib.Either.Left 1>. 
*)
open Either;;
.<Left 1>.;;
(*
- : (int, 'a) t code = .<Stdlib.Either.Left 1>. 
*)
let Left 1 = run .<Left 1>.;;

module Foo = struct exception E end;;
.<raise Foo.E>.;;
(*
Characters 8-13:
  .<raise Foo.E>.;;
          ^^^^^
Exception (extension) Foo.E cannot be used within brackets. Put into a separate file.
*)
print_endline "Error was expected";;

type foo = Bar;;
.<Bar>.;;
(*
Characters 2-5:
  .<Bar>.;;
    ^^^
Unqualified constructor Bar cannot be used within brackets. Put into a separate file.
*)
print_endline "Error was expected";;

module Foo = struct type foo = Bar end;;
.<Foo.Bar>.;;
(*
Characters 2-9:
  .<Foo.Bar>.;;
    ^^^^^^^
Constructor Bar cannot be used within brackets. Put into a separate file.
*)
print_endline "Error was expected";;

(* Records *)

.<{Complex.re = 1.0; im = 2.0}>.;;
(*
- : Complex.t code = .<{ Complex.im = 2.0; Complex.re = 1.0 }>. 
*)
let {Complex.re = 1.; Complex.im = -2.} =
  Complex.conj (run .<{Complex.re = 1.0; im = 2.0}>.);;
let x = {Complex.re = 1.0; im = 2.0} in .<x.re>.;;
(*
The code built at Line 1, characters 40-48 has hardly serializable CSPs: x at Line 1, characters 42-43

It may be allowed in the future: do tell if you need it.
- : float code = .<csp_x_8.Stdlib.Complex.re>. 
*)

let y = {Complex.re = 1.0; im = 2.0} in .<y.Complex.re>.;;
(*
The code built at Line 1, characters 40-56 has hardly serializable CSPs: y at Line 1, characters 42-43

It may be allowed in the future: do tell if you need it.
- : float code = .<csp_y_10.Stdlib.Complex.re>. 
*)
let 1.0 = run(let x = {Complex.re = 1.0; im = 2.0} in .<x.Complex.re>.);;
let x = ref 1 in .<x.contents>.;;       (* Stdlib record *)
(*
- : int code = .<(* CSP x *).Stdlib.contents>. 
*)
let 1 = run(let x = ref 1 in .<x.contents>.);;
let x = ref 1 in .<x.contents <- 2>.;;
(*
The code built at Line 1, characters 17-36 has hardly serializable CSPs: x at Line 1, characters 19-20

It may be allowed in the future: do tell if you need it.
- : unit code = .<csp_x_14.Stdlib.contents <- 2>. 
*)
let x = ref 1 in (run .<x.contents <- 2>.); x;;
(* - : int ref = {contents = 2} *)

open Complex;;
.<{re = 1.0; im = 2.0}>.;;
(*
- : Complex.t code = .<{ Complex.im = 2.0; Complex.re = 1.0 }>. 
*)
let 5.0 = norm (run .<{re = 3.0; im = 4.0}>.);;
let x = {re = 1.0; im = 2.0} in .<x.re>.;;
(*
- : float code = .<(* CSP x *).Complex.re>. 
*)
let 1.0 = run(let x = {re = 1.0; im = 2.0} in .<x.re>.);;

let x = {re = 1.0; im = 2.0} in .<{x with re = 2.0}>.;;
(*
- : Complex.t code = .<{ (* CSP x *) with Complex.re = 2.0 }>. 
*)
let 5.0 = norm @@ run(let x = {re = 4.0; im = 2.0} in .<{x with im = 3.0}>.);;

let 5.0 = norm @@ run(let x = {re = 2.0; im = 4.0} in .<{x with re = 3.0}>.);;

let 5.0 = norm @@ run(let x = {re = 1.0; im = 1.0} in 
   .<{x with re = 3.0; im = 4.0 }>.);;
(*
Characters 56-84:
     .<{x with re = 3.0; im = 4.0 }>.);;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 23: all the fields are explicitly listed in this record:
the 'with' clause is useless.
*)

type foo = {fool : int};;
.<{fool = 1}>.;;
(*
Characters 3-7:
  .<{fool = 1}>.;;
     ^^^^
Unqualified label fool cannot be used within brackets. Put into a separate file.
*)
print_endline "Error was expected";;

(* Conditional *)

.<if true then 1 else 2>.;;
(* - : int code = .<if true then 1 else 2>. *)
.<if Some 1 = None then print_string "weird">.;;
(*
- : unit code = .<if (Some 1) = None then Stdlib.print_string "weird">. 
*)
let () = run .<if Some 1 = None then print_string "weird">.;;

(* Polymorphic variants *)
.<`Foo>.;;
(*
- : [> `Foo ] code = .<`Foo>. 
*)
.<`Bar 1>.;;
(*
- : [> `Bar of int ] code = .<`Bar 1>. 
*)
let 1 = match run .<`Bar 1>. with `Bar x -> x ;;

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
- : int code = .<(* CSP x *)#foo>. 
*)
let 1 = run (f x);;

(* Local open *)
.<Complex.(norm {re=3.0; im = 4.0})>.;;
(*
- : float code = .<Complex.norm { Complex.im = 4.0; Complex.re = 3.0 }>. 
*)

let 5.0 = run .<Complex.(norm {re=3.0; im = 4.0})>.;;

.<let open Complex in norm {re=4.0; im = 3.0}>.;;
(*
- : float code = .<Complex.norm { Complex.re = 4.0; Complex.im = 3.0 }>. 
*)

let 5.0 = run .<let open Complex in norm {re=4.0; im = 3.0}>.;;

(* For-loop *)

.<for i=1 to 5 do Printf.printf "ok %d %d\n" i (i+1) done>.;;
(*
- : unit code = .<
for i_1 = 1 to 5 do Printf.printf "ok %d %d\n" i_1 (i_1 + 1) done>. 
*)
run .<for i=1 to 5 do Printf.printf "ok %d %d\n" i (i+1) done>.;;
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
run .<for i=5 downto 1 do Printf.printf "ok %d %d\n" i (i+1) done>.;;
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
run .<for i=1 to 2 do 
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
run c;;
(*
ok 1 1
ok 2 1
ok 3 1
ok 1 2
ok 2 2
ok 3 2
*)

(* Anonymous loop variable (new in 4.02?) *)
.<for _ = 1 to 3 do print_string "ok" done>.;;
(*
- : unit code = .<for _for_11 = 1 to 3 do Stdlib.print_string "ok" done>. 
*)
run .<for _ = 1 to 3 do print_string "ok" done>.;;
(*
okokok- : unit = ()
*)

(* Scope extrusion test *)

.<for i=1 to 10 do  .~(let _ = run .<0>. in .<()>.) done>.;;
(*
- : unit code = .<for i_2 = 1 to 10 do () done>. 
*)
.<for i=1 to 10 do  .~(let _ = run .<i>. in .<()>.) done>.;;
(*
Exception:
Failure
 "The code built at Characters 6-7:\n  .<for i=1 to 10 do  .~(let _ = run .<i>. in .<()>.) done>.;;\n        ^\n is not closed: identifier i_3 bound at Characters 6-7:\n  .<for i=1 to 10 do  .~(let _ = run .<i>. in .<()>.) done>.;;\n        ^\n is free".

Was:
Characters 14-22:
  .<fun x -> .~(run .<x>.; .<1>.)>.;;
                ^^^^^^^^
Error: run error: 'cl not generalizable in ('cl, 'a) code
*)
print_endline "Error was expected";;

let r = ref .<0>. in ignore (.<for i=1 to 5 do .~(r := .<0>.; .<()>.) done>.); 
                     .<for i=1 to 5 do ignore (.~(!r)) done>.;;
(*
- : unit code = .<for i_13 = 1 to 5 do Stdlib.ignore 0 done>. 
*)

let r = ref .<0>. in ignore (.<for i=1 to 5 do .~(r := .<i>.; .<()>.) done>.); 
                     .<for i=1 to 5 do ignore (.~(!r)) done>.;;
(*
  Exception:
Failure
 "Scope extrusion detected at Line 2, characters 39-54 for code built at Line 1, characters 35-36 for the identifier i_44 bound at Line 1, characters 35-36\nThe problematic code is shown below\n\ni_44".
*)
print_endline "Error was expected";;

(* Better error message *)
let r = ref .<0>. in ignore (.<for i=1 to 5 do .~(r := .<i+1>.; .<()>.) done>.); 
                     .<for i=1 to 5 do ignore (.~(!r)) done>.;;

(*
  Exception:
Failure
 "Scope extrusion detected at Line 2, characters 39-54 for code built at Line 1, characters 55-62 for the identifier i_46 bound at Line 1, characters 35-36\nThe problematic code is shown below\n\ni_46 + 1".

*)
print_endline "Error was expected";;

(* Simple functions *)
.<fun x -> x>.;;
(*
- : ('a -> 'a) code = .<fun x_7  -> x_7>. 
*)
let 42 = (run .<fun x -> x>.) 42;;

.<fun x y -> x + y>.;;
(*
- : (int -> int -> int) code = .<fun x_9  y_10  -> x_9 + y_10>. 
*)
let 5 = (run .<fun x y -> x + y>.) 2 3;;

.<fun x -> fun x -> x + x >.;;
(*
- : ('a -> int -> int) code = .<fun x_13  x_14  -> x_14 + x_14>. 
*)
let 8 = run .<fun x -> fun x -> x + x >. 3 4;;

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
let 5 = (run (eta (fun y -> .<fun x -> x + .~y>.))) 2 3;;

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
let 7 = run (fhyg .<1>. 3);;

(* Optional arguments *)
let t = .< fun ?(x=0) y -> x + y >.;;
(*
val t : (?x:int -> int -> int) code = .<
  fun ?x:xoptx_1 ->
    let x_3 = match xoptx_1 with | Some xsthx_2 -> xsthx_2 | None -> 0 in
    fun y_4 -> x_3 + y_4>.
  
*)
let 1 = (run t) 1;;
let 3 = (run t) ~x:2 1;;

(* Type ascriptions *)

let t = .<fun x -> (x:int)>.
(*
val t : (int -> int) code = .<fun x_3 -> x_3>. 
*)
let 5 = run t 5;;

(*
 .<fun (x:int) -> x>. 
 is not yet supported
 But the following is OK, for documentation purposes.
*)

let t = .<let f : int -> int = fun x -> x in f 1>.
(*
val t : int code = .<let f_2 x_1 = x_1 in f_2 1>. 
*)
let 1 = run t;;

(* pattern-matching, general functions *)

.<fun () -> 1>.;;
(* - : (unit -> int) code = .<fun () -> 1>. *)
run .<fun () -> 1>.;;
(* - : unit -> int = <fun> *)
let 1 = (run .<fun () -> 1>.) ();;

.<function true -> 1 | false -> 0>.;;
(*
- : (bool -> int) code = .<function | true -> 1 | false -> 0>. 
*)
let 1 = (run .<function true -> 1 | false -> 0>.) true;;

.<fun (true,[]) -> 1>.;;
(*
- : (bool * 'a list -> int) code = .<fun (true, []) -> 1>. 
*)
(run .<fun (true,[]) -> 1>.) (true,[1]);;
(*
Exception: Match_failure ("//toplevel//", 1, 6).
*)
print_endline "Error was expected";;
let 1 = (run .<fun (true,[]) -> 1>.) (true,[]);;

.<fun [|true;false;false|] -> 1>.;;
(*
- : (bool array -> int) code = .<fun [|true; false; false|] -> 1>. 
*)
let 1 = (run .<fun [|true;false;false|] -> 1>.) [|true;false;false|];;

.<function `F 1 -> true | _ -> false>.;;
(*
- : ([> `F of int ] -> bool) code = .<function | `F 1 -> true | _ -> false>. 
*)
let true = (run .<function `F 1 -> true | _ -> false>.) (`F 1);;
.<function `F 1 | `G 2 -> true | _ -> false>.;;
(*
- : ([> `F of int | `G of int ] -> bool) code = .<
function | `F 1|`G 2 -> true | _ -> false>. 
*)

.<function (1,"str") -> 1 | (2,_) -> 2>.;;
(*
- : (int * string -> int) code = .<function | (1,"str") -> 1 | (2,_) -> 2>. 
*)
let 1 = (run .<function (1,"str") -> 1 | (2,_) -> 2>.) (1,"str");;
let 2 = (run .<function (1,"str") -> 1 | (2,_) -> 2>.) (2,"str");;
let 1 = (run .<fun [1;2] -> 1>.) [1;2];;

let 2 = (run .<function None -> 1 | Some [1] -> 2>.) (Some [1]);;

let 2 = (run .<function (Some (Some true)) -> 1 | _ -> 2>.) (Some None);;
let 1 = (run .<function (Some (Some true)) -> 1 | _ -> 2>.) (Some (Some true));;
let 2 = (run .<function (Some (Some true)) -> 1 | _ -> 2>.) (Some (Some false));;
let 2 = (run .<function (Some (Some true)) -> 1 | _ -> 2>.) None;;

open Complex;;
.<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.;;
(*
- : (Complex.t -> int) code = .<
function
| { Complex.re = 1.0 } -> 1
| { Complex.re = 2.0; Complex.im = 2.0 } -> 2
| { Complex.im = _ } -> 3>. 
*)

let 1 = (run .<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.)
        {re=1.0; im=2.0};;
let 2 = (run .<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.)
        {re=2.0; im=2.0};;
(* - : int = 2 *)
let 3 = (run .<function {re=1.0} -> 1 | {im=2.0; re = 2.0} -> 2 | {im=_} -> 3>.)
        {re=2.0; im=3.0};;

(* General functions *)

(* Non-binding pattern *)
.<fun () -> 1>.;;
(*
- : (unit -> int) code = .<fun ()  -> 1>. 
*)
let 1 = run .<fun () -> 1>. ();;

(* .<fun (type a) () -> 1>.;; *)

.<fun _ -> true>.;;
(* - : ('a -> bool) code = .<fun _  -> true>. *)
let true = run .<fun _ -> true>. 1;;

.<fun (x,y) -> x + y>.;;
(*
- : (int * int -> int) code = .<fun (x_1,y_2)  -> x_1 + y_2>. 
*)
let 5 = (run .<fun (x,y) -> x + y>.) (2,3);;
.<function (Some x) as _y -> x | _ ->  2>.;;
(*
- : (int option -> int) code = .<function | Some x_5 as _y_6 -> x_5 | _ -> 2>. 
*)
let 1 = (run .<function (Some x) as _y -> x | _ ->  2>.) (Some 1);;
let 2 = (run .<function (Some x) as _y -> x | _ ->  2>.) None;;
.<function [x;y;z] -> x - y + z | [x;y] -> x - y>.;;
(*
- : (int list -> int) code = .<
function
| x_11::y_12::z_13::[] -> (x_11 - y_12) + z_13
| x_14::y_15::[] -> x_14 - y_15>. 
*)
let 2 = (run .<function [x;y;z] -> x - y + z | [x;y] -> x - y>.) [1;2;3];;

 (* OR patterns *)
.<function ([x;y] | [x;y;_]) -> x - y>.;;
(*
- : (int list -> int) code = .<
fun (x_21::y_22::[]|x_21::y_22::_::[])  -> x_21 - y_22>. 
*)
let -1 = (run .<function ([x;y] | [x;y;_]) -> x - y>.) [1;2];;
let -1 = (run .<function ([x;y] | [x;y;_]) -> x - y>.) [1;2;3];;
(run .<function ([x;y] | [x;y;_]) -> x - y>.) [1;2;3;4];;
(* Exception: Match_failure ("//toplevel//", 1, 6). *)
print_endline "Error was expected";;

.<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.;;
(*
- : (int list -> int) code = .<
fun (x_29::y_30::[]|x_29::y_30::_::[]|y_30::x_29::_::_::[])  -> x_29 - y_30>. 
*)
let -1 = (run .<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.) [1;2];;
let -1 = (run .<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.) [1;2;3];;
let  1 = (run .<function ([x;y] | [x;y;_]| [y;x;_;_]) -> x - y>.) [1;2;3;4];;

.<function (`F x | `G x) -> x | `E x -> x>.;;
(*
- : ([< `E of 'a | `F of 'a | `G of 'a ] -> 'a) code = .<
function | `F x_37|`G x_37 -> x_37 | `E x_38 -> x_38>. 
*)
let 2 = (run .<function (`F x | `G x) -> x | `E x -> x>.) (`F 2);;
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
let 0. = (run .<function {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.) 
         {re=1.0; im=1.0};;
let 1. = (run .<function {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.) 
         {re=1.0; im=2.0};;
.<function (Some x) as y when x  > 0 -> y | _ -> None>.;;
(*
- : (int option -> int option) code = .<
function | Some x_53 as y_54 when x_53 > 0 -> y_54 | _ -> None>. 
*)
let Some 1 = (run .<function (Some x) as y when x  > 0 -> y | _ -> None>.)
             (Some 1);;
let None = (run .<function (Some x) as y when x  > 0 -> y | _ -> None>.)
           (Some 0);;

(* pattern-matching *)
.<match 1 with 1 -> true>.;;
(*
- : bool code = .<match 1 with | 1 -> true>. 
*)
let true = run .<match 1 with 1 -> true>.;;

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
  run .<match [1;2] with [x] -> x | [x;y] -> x + y>.;;

(* OR patterns *)
.<match [1;2] with [x] -> x | [x;y] | [x;y;_] -> x + y>.;;
(*
- : int code = .<
match [1; 2] with
| x_13::[] -> x_13
| x_14::y_15::[]|x_14::y_15::_::[] -> x_14 + y_15>. 
*)
let 3 = run .<match [1;2] with [x] -> x | [x;y] | [x;y;_] -> x + y>.;;

.<match [1;2;3;4] with [x] -> x | [x;y] | [x;y;_] | [y;x;_;_] -> x - y>.;;
(*
- : int code = .<
match [1; 2; 3; 4] with
| x_19::[] -> x_19
| x_20::y_21::[]|x_20::y_21::_::[]|y_21::x_20::_::_::[] -> x_20 - y_21>. 
*)
let 1 =
  run .<match [1;2;3;4] with [x] -> x | [x;y] | [x;y;_] | [y;x;_;_] -> x - y>.;;

.<fun x -> match x with (`F x | `G x) -> x | `E x -> x>.;;
(*
- : ([< `E of 'a | `F of 'a | `G of 'a ] -> 'a) code = .<
fun x_25  -> match x_25 with | `F x_26|`G x_26 -> x_26 | `E x_27 -> x_27>. 
*)

let 1 = (run .<fun x -> match x with (`F x | `G x) -> x | `E x -> x>.) (`G 1);;

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
  (run .<fun x -> match x with {re=x; im=2.0} -> x | {im=x; re=y} -> x -. y>.)
    {im=2.0; re=1.0};;

(* exceptional cases *)
.<match List.mem 1 [] with x -> x | exception Not_found -> false>.;;
(*
- : bool code = .<
match List.mem 1 [] with | x_95 -> x_95 | exception Not_found  -> false>. 
*)
let false = 
 run .<match List.mem 1 [] with x -> x | exception Not_found -> false>.

let f = .<fun x ->
 match List.assoc 1 x with "1" as x -> x | x -> x
 | exception Not_found -> "" | exception Invalid_argument x -> x>.
(*
val f : ((int * string) list -> string) code = .<
  fun x_100  ->
    match List.assoc 1 x_100 with
    | "1" as x_101 -> x_101
    | x_102 -> x_102
    | exception Not_found  -> ""
    | exception Invalid_argument x_103 -> x_103>.
*)
let "" = run f []
let "1" = run f [(1,"1")]
let "0" = run f [(1,"0")];;

(* try *)
.<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.;;
(*
- : (int -> bool option) code = .<
fun x_39  ->
  try Some (List.assoc x_39 [(1, true); (2, false)])
  with | Not_found  -> None>.
*)
let Some true =
  (run .<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.) 1;;
let Some false =
 (run .<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.) 2;;
let None =
 (run .<fun x -> try Some (List.assoc x [(1,true); (2,false)]) with Not_found -> None>.) 3;;

.<fun x -> let open Scanf in try sscanf x "%d" (fun x -> string_of_int x) with Scan_failure x -> "fail " ^ x>.;;
(*
- : (string -> string) code = .<
fun x_43  ->
  let open Scanf in
    try Scanf.sscanf x_43 "%d" (fun x_44  -> Stdlib.string_of_int x_44)
    with | Scanf.Scan_failure x_45 -> "fail " ^ x_45>.
*)

let "1" = 
  (run .<fun x -> let open Scanf in try sscanf x "%d" (fun x -> string_of_int x) with Scan_failure x -> "fail " ^ x>.) "1";;
let "fail scanf: bad input at char number 0: character 'x' is not a decimal digit" =
 (run .<fun x -> let open Scanf in try sscanf x "%d" (fun x -> string_of_int x) with Scan_failure x -> "fail " ^ x>.) "xxx";;

(* Simple let *)

.<let x = 1 in x>.;;
(*
- : int code = .<let x_1 = 1 in x_1>. 
*)
let 1 = 
  run .<let x = 1 in x>.;;
.<let x = 1 in let x = x + 1 in x>.;;
(*
- : int code = .<let x_55 = 1 in let x_56 = x_55 + 1 in x_56>. 
*)
let 2 = 
  run .<let x = 1 in let x = x + 1 in x>.;;
.<let rec f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;
(*
- : int code = .<
let rec f_7 n_8 = if n_8 = 0 then 1 else n_8 * (f_7 (n_8 - 1)) in f_7 5>. 
*)
let 120 =
  run .<let rec f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;

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

let 120 = run .<let f = fun x -> x in 
               let rec f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;

.<let f = fun x -> x in 
  let f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;
(*
  - : int code = .<
let f_20 x_19 = x_19 in
let f_22 n_21 = if n_21 = 0 then 1 else n_21 * (f_20 (n_21 - 1)) in f_22 5>. 
*)
let 20 = run .<let f = fun x -> x in 
              let f = fun n -> if n = 0 then 1 else n * f (n-1) in f 5>.;;

.<let g = fun x -> x+10 in
  let f = fun x -> g x + 20
  and g = fun n -> if n = 0 then 1 else n * g (n-1) in (f 5,g 5)>.;;

(*
- : (int * int) code = .<
let g_28 x_27 = x_27 + 10 in
let f_29 x_32 = (g_28 x_32) + 20
and g_30 n_31 = if n_31 = 0 then 1 else n_31 * (g_28 (n_31 - 1)) in
((f_29 5), (g_30 5))>. 
*)
let (35,70) = run .<let g = fun x -> x+10 in
  let f = fun x -> g x + 20
  and g = fun n -> if n = 0 then 1 else n * g (n-1) in (f 5,g 5)>.;;

.<let g = fun x -> x+10 in
  let rec f = fun x -> g x + 20
  and g = fun n -> if n = 0 then 1 else n * g (n-1) in (f 5,g 5)>.;;

(*
Characters 6-7:
  .<let g = fun x -> x+10 in
        ^
Warning 26: unused variable g.
- : (int * int) code = .<
let g_40 x_39 = x_39 + 10 in
let rec f_41 x_44 = (g_42 x_44) + 20
and g_42 n_43 = if n_43 = 0 then 1 else n_43 * (g_42 (n_43 - 1)) in
((f_41 5), (g_42 5))>. 
*)

let (140,120) =
 run .<let g = fun x -> x+10 in
  let rec f = fun x -> g x + 20
  and g = fun n -> if n = 0 then 1 else n * g (n-1) in (f 5,g 5)>.;;

.<let rec [] = [] in []>.;;
(*
Characters 10-12:
  .<let rec [] = [] in []>.;;
            ^^
Only variables are allowed as left-hand side of `let rec'
*)
print_endline "Error was expected";;

.<let rec f = f in f>.;;
(*
Line 1, characters 14-15:
1 | .<let rec f = f in f>.;;
                  ^
Error: This kind of expression is not allowed as right-hand side of `let rec'
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
let true = (run .<fun x -> let rec even = function 0 -> true | x -> odd (x-1) and odd = function 0 -> false | x -> even (x-1) in even x>.) 42;;
let false = (run .<fun x -> let rec even = function 0 -> true | x -> odd (x-1) and odd = function 0 -> false | x -> even (x-1) in even x>.) 43;;


(* General let *)
.<let x = 1 and y = 2 in x + y>.;;
(*
- : int code = .<let x_73 = 1 and y_74 = 2 in x_73 + y_74>. 
*)
let 3 = run .<let x = 1 and y = 2 in x + y>.;;

.<let x = 1 in let x = x+1 and y = x+1 in x + y>.;;
(*
- : int code = .<
let x_77 = 1 in let x_78 = x_77 + 1 and y_79 = x_77 + 1 in x_78 + y_79>. 
*)
let 4 = run .<let x = 1 in let x = x+1 and y = x+1 in x + y>.;;
.<fun x -> let (Some x) = x in x + 1>.;;
(*
- : (int option -> int) code = .<
fun x_83  -> let Some x_84 = x_83 in x_84 + 1>. 
*)
let 3 = (run .<fun x -> let (Some x) = x in x + 1>.) (Some 2);;
(run .<fun x -> let (Some x) = x in x + 1>.) None;;
(*
Exception: Match_failure ("//toplevel//", 1, 19).
*)
print_endline "Error was expected";;



(* testing scope extrusion *)
let r = ref .<0>. in let _ = .<fun x -> .~(r := .<1>.; .<0>.)>. in !r ;;
(* - : int code = .<1>.  *)
let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in !r ;;
(*
- : int code = .<x_279>.

Failure("The code built at Line 1, characters 35-36 is not closed: identifier x_279 bound at Line 1, characters 35-36 is free")
*)
print_endline "Error was expected";;

let c = let r = ref .<0>. in let _ = .<fun x -> .~(r := .<x>.; .<0>.)>. in (!r) in run c;;
(*
Exception:
Failure
 "The code built at Line 1, characters 43-44 is not closed: identifier x_280 bound at Line 1, characters 43-44 is free".
*)
print_endline "Error was expected";;

let r = ref .<fun y->y>. in let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in !r ;;
(*
- : ('_weak2 -> '_weak2) code = .<fun y_283 -> x_282>.

Failure("The code built at Line 1, characters 55-69 is not closed: identifier x_282 bound at Line 1, characters 42-43 is free")
*)
print_endline "Error was expected";;

(* Error message is reported on splice *)
let r = ref .<fun y->y>. in 
 let _ = .<fun x -> .~(r := .<fun y -> x>.; .<0>.)>. in .<fun x -> .~(!r) 1>. ;;
(*
  Exception:
Failure
 "Scope extrusion detected at Line 2, characters 67-75 for code built at Line 2, characters 28-42 for the identifier x_285 bound at Line 2, characters 15-16\nThe problematic code is shown below\n\nfun y_286 -> x_285".
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
 let r = ref .<fun y->y>. in run .<fun x -> .~(r := .<fun y -> x>.; !r)>. 3 4;;

(* Attributes *)
let t = .<fun x -> (x+1)[@foo]>.
(*
val t : (int -> int) code = .<fun x_4 -> ((x_4 + 1)[@foo ])>. 
*)
let t = .<fun x -> (x+1)[@ppwarning "todo"]>.
(*                                        ^^^^^^
Warning 22: todo
val t : (int -> int) code = .<fun x_5 -> ((x_5 + 1)[@ppwarning "todo"])>. 
*)

let t = .<let f x = x [@@inline] in (f[@inlined]) ()>.

(*
let f x = x [@@inline] in (f[@inlined]) ()
    .<(1+2)[@metaocam.annot ()[@foo]]>.
    .<(1+2)[@metaocam.annot : int]>.

.<let module M = Option in 1>.


let t = .<fun x -> let[@warning "-8"] [] = x in 1>.
*)


let _ = print_endline "\nAll done\n";;
