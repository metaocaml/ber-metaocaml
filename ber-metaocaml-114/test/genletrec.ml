(* The test cases for (mutually) recursive genletrec *)
[@@@warning "-8"]
;;

(* Standard Fibonacci (Gibonacci) *)
let fib x y n =
  let rec loop n = 
    if n = 0 then x else if n = 1 then y else loop (n-1) + loop (n-2)
  in loop n
let[@warning "-8"] 8 = fib 1 1 5

(* Staged and specialized to n. First, naive specialization *)
let sfib n =
  let rec loop n (x,y) = 
    if n = 0 then x else if n = 1 then y else 
    .<.~(loop (n-1) (x,y)) + .~(loop (n-2) (x,y))>.
  in .<fun x y -> .~(loop n (.<x>.,.<y>.))>.

let _ = sfib 5
;;

(*
- : (int -> int -> int) code = .<
fun x_1 ->
  fun y_2 -> (((y_2 + x_1) + y_2) + (y_2 + x_1)) + ((y_2 + x_1) + y_2)>.
*)

let sfib n =
  with_locus_rec @@ fun l ->
  let g = mkgenlet l (=) in
  let rec loop n = 
    if n = 0 then .<fun (x,y) -> x>. else if n = 1 then .<fun (x,y) -> y>. else 
    .<fun (x,y) -> .~(g loop (n-1)) (x,y) + .~(g loop (n-2)) (x,y)>.
  in .<fun x y -> .~(g loop n) (x,y)>.
;;
let c = sfib 5
;;
(*
c : (int -> int -> int) code = .<
let rec h_21 (x_22, y_23) = (h_9 (x_22, y_23)) + (h_15 (x_22, y_23))
and h_18 (x_19, y_20) = x_19
and h_15 (x_16, y_17) = (h_12 (x_16, y_17)) + (h_18 (x_16, y_17))
and h_12 (x_13, y_14) = y_14
and h_9 (x_10, y_11) = (h_15 (x_10, y_11)) + (h_12 (x_10, y_11))
and h_6 (x_7, y_8) = (h_21 (x_7, y_8)) + (h_9 (x_7, y_8)) in
fun x_4 -> fun y_5 -> h_6 (x_4, y_5)>. 
*)

let[@warning "-8"] 8 = Runcode.run c 1 1


(* Neil Jones benchmark: specializing the Ackermann function *)
let rec ack m n =
  if m = 0 then n+1 else
  if n = 0 then ack (m-1) 1 else 
  ack (m-1) (ack m (n-1))
;;
let[@warning "-8"] 9 = ack 2 3;;
(* A(2,y) = 2y+3  A(1,y)=y+2 A(3,y)=2^(n+3)-3 *)

(* Specialized to m. Without memoization, it diverges *)
let sack m =
  with_locus_rec @@ fun l ->
  let g = mkgenlet l (=) in
  let rec loop m =
    if m = 0 then .<fun n -> n + 1>. else
    .<fun n -> if n = 0 then .~(g loop (m-1)) 1
    else .~(g loop (m-1)) (.~(g loop m) (n-1))>.
  in g loop m
;;
let sac2 = sack 2
;;
(*
val sac2 : (int -> int) code = .<
  let rec h_50 n_51 = n_51 + 1
  and h_48 n_49 = if n_49 = 0 then h_50 1 else h_50 (h_48 (n_49 - 1))
  and h_46 n_47 = if n_47 = 0 then h_48 1 else h_48 (h_46 (n_47 - 1)) in h_46>.
  
*)
let[@warning "-8"] 9 = Runcode.run sac2 3;;

(* Generalized Even-Odd *)
(*
    let rec f_o x = x  = 0 || f_{n-1} (x-1)
        and f_1 x = x <> 0 && f_o (x-1)
        ...
        and f_{ₙ₋₁} x = x <> 0 && f_{ₙ₋₂} (x-1)
*)

let mkodds m = 
  with_locus_rec @@ fun l ->
  let g = mkgenlet ~name:"Ex" l (=) in
  let rec loop i = .<fun n ->
   .~(if i = 0 then 
       .<n = 0  || .~(g loop (m-1)) (n-1)>. else
       .<n <> 0 && .~(g loop (i-1)) (n-1)>.)>.
  in g loop 0
;;
let odd3 = mkodds 3
;;
(*
val odd3 : (int -> bool) code = .<
  let rec xx_57 n_58 = (n_58 <> 0) && (xx_53 (n_58 - 1))
  and xx_55 n_56 = (n_56 <> 0) && (xx_57 (n_56 - 1))
  and xx_53 n_54 = (n_54 = 0) || (xx_55 (n_54 - 1)) in xx_53>. 
*)

let[@warning "-8"] [true; false; false; true; false]
  = List.map (Runcode.run odd3) [0;1;2;3;4]

;;

(* Finite state automaton *)
type state2 = S1 | S2 | S3 | S4

type ('alph,'state) automaton = 
    {finals : 'state list;
     trans  : ('state * ('alph * 'state) list) list
   }
let au3 = 
  {finals = [S1; S2; S3];
   trans = [S1, [`A, S2; `B, S3];
            S2, [`A, S3; `B, S4];
            S3, [`A, S4; `B, S1];
            S4, [`A, S1; `B, S2];]
 }
;;

let makeau : ('alph, 'state) automaton -> 'state -> ('alph list -> bool) code =
  fun {finals;trans} state ->
  with_locus_rec @@ fun l ->
  let g = mkgenlet ~name:"st" l (=) in
  let rec loop state =
    let accept = List.mem state finals in 
    let next token = List.assoc token (List.assoc state trans) in
    .<function
      | `A :: r -> .~(g loop (next `A)) r 
      | `B :: r -> .~(g loop (next `B)) r 
      | [] -> accept>.
  in g loop state
 ;;

let _ = makeau au3 S1
;;
(*
- : ([ `A | `B ] list -> bool) code = .<
let rec st_84 =
  function | `A::r_85 -> st_78 r_85 | `B::r_86 -> st_81 r_86 | [] -> true
and st_81 =
  function | `A::r_82 -> st_75 r_82 | `B::r_83 -> st_84 r_83 | [] -> false
and st_78 =
  function | `A::r_79 -> st_81 r_79 | `B::r_80 -> st_75 r_80 | [] -> true
and st_75 =
  function | `A::r_76 -> st_84 r_76 | `B::r_77 -> st_78 r_77 | [] -> true in
st_75>. 
*)

(* Fully generic automatoc compiler, for any alphabet and state *)
let makeau : ('alph, 'state) automaton -> 
             ('alph -> 'alph code) ->   (* lifting alphabet *)
             'state -> ('alph list -> bool) code =
  fun {finals;trans} lift state ->
  with_locus_rec @@ fun l ->
  let g = mkgenlet ~name:"st" l (=) in
  let rec loop state =
    let accept = List.mem state finals in
    .<fun[@warning "-8"] l -> .~(make_match .<l>. @@
       .<fun [] -> accept>. [@metaocaml.functionliteral] ::
         (List.assoc state trans |>
          List.map (fun (a,s) ->
           .<function x::r when x = .~(lift a) -> .~(g loop s) r>.
             [@metaocaml.functionliteral]))
           )>.
  in g loop state
 ;;

let _ = makeau au3 (function `A -> .<`A>. | `B -> .<`B>.) S1
;;

(*
- : ([ `A | `B ] list -> bool) code = .<
let rec st_20 l_21 =
  match l_21 with
  | [] -> false
  | x_22::r_23 when x_22 = `A -> st_8 r_23
  | x_24::r_25 when x_24 = `B -> st_12 r_25
and st_16 l_17 =
  match l_17 with
  | [] -> true
  | x_18::r_19 when x_18 = `A -> st_20 r_19
  | x_26::r_27 when x_26 = `B -> st_8 r_27
and st_12 l_13 =
  match l_13 with
  | [] -> true
  | x_14::r_15 when x_14 = `A -> st_16 r_15
  | x_28::r_29 when x_28 = `B -> st_20 r_29
and st_8 l_9 =
  match l_9 with
  | [] -> true
  | x_10::r_11 when x_10 = `A -> st_12 r_11
  | x_30::r_31 when x_30 = `B -> st_16 r_31 in
st_8>. 
*)

let _ = print_endline "\nAll done\n";;
