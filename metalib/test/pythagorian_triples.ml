(* Test of the let! notation: Fair monad for non-determinism *)

module NonDet : sig
  type 'a stream_v
  type 'a stream = unit -> 'a stream_v

  val ret  : 'a -> 'a stream
  val fail : 'a stream

      (* a.k.a bind or disjunction *)
  val (let!) : 'a stream -> ('a -> 'b stream) -> 'b stream
      (* a.k.a. fair disjunction *)
  val mplus  : 'a stream -> 'a stream -> 'a stream

  val guard  : bool -> unit stream
  val yield  : 'a stream -> 'a stream

  val run : int ->             (* upper bound on the number of solutions *)
            'a stream ->
	    'a list
end = struct
  type 'a stream_v = 
    Nil | Cons of 'a * 'a stream | InC of 'a stream
  and 'a stream = unit -> 'a stream_v

  let fail  = fun () -> Nil
  let ret a = fun () -> Cons (a,fail)

  (* actually, interleave: a fair disjunction with breadth-first search*)
  let rec mplus a b = fun () -> 
    match a () with
    | Nil          -> InC b
    | Cons (a1,a2) -> Cons (a1,(mplus b a2))
    | InC a -> 
	begin match b () with
	| Nil   -> InC a
        | InC b -> InC (mplus a b)
        | Cons (b1,b2) -> Cons (b1, (mplus a b2))
	end

  (* a fair conjunction *)
  let rec (let!) m f = fun () -> 
    match m () with
    | Nil        -> fail ()
    | InC a      -> InC ((let!) a f)
    | Cons (a,b) -> mplus (f a) ((let!) b f) ()

  let guard be = if be then ret () else fail
  let yield m  () = InC m

  let rec run n m = 
    if n = 0 then [] else
    match m () with
    | Nil -> []
    | InC a -> run n a
    | Cons (a,b) -> (a::run (n-1) b)
end;;

open NonDet;;

(* The example uses left recursion and truly infinite streams! *)
(* Don't try this in Prolog or in Haskell's MonadPlus. *)

let rec numb () = 			(* infinite stream of integers *)
    yield (mplus (let! n = numb in ret (n+1))         (* left recursion! *)
	       (ret 0)) ()
;;

let pyth : (int * int * int) NonDet.stream =
  let! i  = numb in
  let! () = guard (i>0) in
  let! j  = numb in
  let! () = guard (j>0) in
  let! k  = numb in
  let! () = guard (k>0) in
  (* Just to illustrate the `let' form within let! *)
  let test x = x*x = j*j + k*k in
  let! () = guard (test i) in
  ret (i,j,k)
;;

let [(5, 4, 3); (5, 3, 4); (10, 8, 6); (10, 6, 8); (13, 12, 5); (13, 5, 12);
     (15, 12, 9); (15, 9, 12); (17, 15, 8); (17, 8, 15)]
 =
run 10 pyth;;

print_endline "\nAll done";;
