(* Generation of more efficient letrec code, without nesting of letrec *)
(* This is the product of the discussion with Jun Inoue in Oct 2013 *)

module type LETRECS = sig
  type letrec_id                        (* abstract *)
  val make_letrecs : (letrec_id -> 'w code) -> 'w code

  val add_letrec : letrec_id -> 
    (('a->'b) code -> ('a->'b) code * 'w) -> 'w
end

    (* (('cl,'a->'b) code -> ('cl,'w) code) -> ('cl,'w) code *)

(* Example of using the interface
   First, the standard even-odd example
 *)

module Ex1(S:LETRECS) = struct
  open S

  let r =
    make_letrecs @@ fun lid ->
      add_letrec lid (fun even' ->
      add_letrec lid (fun odd'  ->
        let even = .<fun n -> n = 0 || .~odd' (n-1)>.
        and odd  = .<fun n -> not (n=0) && .~even' (n-1)>.
        in (odd, (even, .<fun n -> (.~odd n, .~even n)>.))
  ))
end

(*
 Next example:
   It is the variation of the even-odd example; the mutually
   recursive functions are (artificially) made to have distinct types

 let rec even = fun n -> n=0 || odd even (n-1)
 and odd even = not (n=0) && even (n-1)
 in fun n -> [even n, odd n]

The next two examples are to generalize the above to compute residuals of k
for example

let rec three0 n = n=0 || three2 (n-1)
and     three1 n = not (n=0) && three0 (n-1)
and     three2 n = not (n=0) && three1 (n-1)


*)
module Ex2(S:LETRECS) = struct
  open S

  let r k =
    make_letrecs @@ fun lid ->
      let rec loop = function
        | k when k <= 0 -> .<[| |]>.
        | 1 -> .<[| true |]>.
        | k -> 
      add_letrec lid (fun even' ->
      add_letrec lid (fun odd'  ->
        let even = .<fun n -> n = 0 || .~odd' (n-1)>.
        and odd  = .<fun n -> not (n=0) && .~even' (n-1)>.
        in (odd, (even, .<fun n -> (.~odd n, .~even n)>.))
  ))
end


(* One, naive implementation: a simultaneous letrec as nested letrecs *)
module Nested : LETRECS = struct
  type letrec_id = unit

  let add_letrec : letrec_id -> 
  (('cl,'a->'b) code -> ('cl,'a->'b) code) -> 
    (('cl,'a->'b) code -> ('cl,'w) code) -> ('cl,'w) code =
  fun lid exp body -> .<let rec x = .~(exp .<x>.) in .~(body .<x>.)>.
   
  val make_letrecs : (letrec_id -> 'w code) -> 'w code
    fun body -> body ()
end;;

