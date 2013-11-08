
(* Author:  Walid Taha and Cristiano Calcagno
   Date:    Fri Aug 31 03:03:11 EDT 2001 *)

open T4types;;

let unJ(J i) = i
let unF(F f) = f

let term0 = A(L("x",V"x"),I 7)

let term1 = A(R ("f","n", C(V "n",I 42,A (V "f",D (V "n")))),I 1000000)

exception Yiikes
;;

let env0 = fun x -> raise Yiikes;;

let ext env x v = fun y -> if x=y then v else env y

let rec eval e env =
match e with
  I i -> J i
| V s -> env s
| A (e1,e2) -> (unF(eval e1 env)) (eval e2 env)
| L (x,e) -> F (fun v -> eval e (ext env x v))
| D e -> J ((unJ (eval e env))-1)
| C (e1,e2,e3) -> if (unJ(eval e1 env))=0 
                     then (eval e2 env)
                     else (eval e3 env)
| R (f,x,e) -> F (let rec ff xx = eval e (ext (ext env x xx)
                                              f (F ff))
                  in ff)
;;

let J 42 = eval term1 env0;; (* Unstaged *)

let rec eval' e env =
match e with
  I i -> .<J i>.
| V s -> env s
| A (e1,e2) -> .<(unF .~(eval' e1 env)) (.~(eval' e2 env))>.
| L (x,e) -> .<F (fun v -> .~(eval' e (ext env x .<v>.)))>.
| D e -> .<J (unJ .~(eval' e env) - 1)>.
| C (e1,e2,e3) -> .< if (unJ .~(eval' e1 env)) = 0 
                              then .~(eval' e2 env)
                              else .~(eval' e3 env) >.
| R (f,x,e) ->
  .<F (let rec ff xx = .~(eval' e (ext (ext env x .<xx>.) f .<F ff>.)) in ff)>.
;;

let stage1Running = eval' term1 env0;;

(*
val stage1Running : ('a, dom) code =
  .<((((* cross-stage persistent value (as id: unF) *))
     (F
       (let rec ff_1 =
         fun xx_2 ->
          if ((((* cross-stage persistent value (as id: unJ) *)) xx_2) = 0) then
           (J (42))
          else
           ((((* cross-stage persistent value (as id: unF) *)) (F (ff_1)))
             (J
               ((((* cross-stage persistent value (as id: unJ) *)) xx_2) - 1))) in
        ff_1))) (J (1000000)))>.

*)
let compiling = .! .<fun () -> .~ stage1Running>.;;

let J 42 = compiling ();;

let unJ' = .<fun (J i) -> i>.
let unF' = .<fun (F f) -> f>.

let rec eval'' e env =
match e with
  I i -> .<J i>.
| V s -> env s
| A (e1,e2) -> .<.~unF' .~(eval'' e1 env) .~(eval'' e2 env)>.
| L (x,e) -> .<F (fun v -> .~(eval'' e (ext env x .<v>.)))>.
| D e -> .<J (.~unJ' .~(eval'' e env) - 1)>.
| C (e1,e2,e3) -> .<if .~unJ' .~(eval'' e1 env) = 0 
                              then .~(eval'' e2 env)
                              else .~(eval'' e3 env)>.
| R (f,x,e) ->
  .<F (let rec ff xx = .~(eval'' e (ext (ext env x .<xx>.) f (.<F ff>.))) in ff)>.
;;

let stage1Running' = eval'' term1 env0;;

(*
val stage1Running' : ('a, dom) code =
  .<((fun F (f_2) -> f_2)
    (F
      (let rec ff_1 =
        fun xx_2 ->
         if (((fun J (i_1) -> i_1) xx_2) = 0) then (J (42))
         else
          ((fun F (f_2) -> f_2) (F (ff_1))
            (J (((fun J (i_1) -> i_1) xx_2) - 1))) in
       ff_1)) (J (1000000)))>.
*)

let compiling' = .! .<fun () -> .~ stage1Running'>.;;

let J 42 = (compiling' ());;

Printf.printf "\n4.ml Done\n";;
