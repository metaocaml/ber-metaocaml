(* Complex example of code extrusion, which could benefit from better
   diagnostics (especially when working at the top-level).
*)

open Print_code

let genletrec : (('a->'b) code -> 'a code -> 'b code) -> ('a->'b) code = 
  fun f -> genlet .<let rec g x = .~(f .<g>. .<x>.) in g>.

let rec ack m n =
  if m = 0 then n+1 else
  if n = 0 then ack (m-1) 1
  else ack (m-1) (ack m (n-1))
;;
let 9 = ack 2 3;;
(* A(2,y) = 2y+3  A(1,y)=y+2 A(3,y)=2^(n+3)-3 *)

let tack self m =
  if m = 0 then (fun n -> .<.~n+1>.) else
  fun n -> .<if .~n = 0 then .~(self (m-1)) 1
  else .~(self (m-1)) (.~(self m) (.~n-1))>.
;;

let mrfix : (('a -> 'b code) -> ('a -> 'b code)) -> ('a -> 'b code) = 
  fun f x ->
  let memo = ref [] in
  let rec loop n = 
    match List.assoc n !memo with
    | x -> x
    | exception Not_found -> 
        genletrec (fun g y -> 
        memo := (n,g) :: !memo;
        let v = (f loop n) in
        .<.~v .~y>.)
  in loop x

let _ = mrfix (fun self m -> .<fun n -> .~(tack self m .<n>.)>.) 1;;
