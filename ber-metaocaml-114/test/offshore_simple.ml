(* Simple test of offshoring: testing the IR *)

(*
#load "offshoring.cma";;
*)

open OffshoringIR
open Offshoring

let _ = offshore (module DefaultConv) .<fun x -> x + 1>.
;;
(*
- : proc_t =
([("x_1", OffshoringIR.TNum I32)], OffshoringIR.TNum I32,
 Block (<abstr>,
  Exp
   (FunCall (OffshoringIR.OP.ADD I32,
     [LocalVar "x_1"; Const (Const_num (I32, "1"))]))))
*)

let _ = offshore (module DefaultConv) .<fun x y -> x * y + 1>.
;;
(*
- : proc_t =
([("x_2", OffshoringIR.TNum I32); ("y_3", OffshoringIR.TNum I32)],
 OffshoringIR.TNum I32,
 Block (<abstr>,
  Exp
   (FunCall (OffshoringIR.OP.ADD I32,
     [FunCall (OffshoringIR.OP.MUL I32, [LocalVar "x_2"; LocalVar "y_3"]);
      Const (Const_num (I32, "1"))]))))
*)

let _ = 
  try ignore (offshore (module DefaultConv) .<fun x -> succ @@ x>.);
  assert false
  with Failure e -> e
;;
(*
      - : string = "Don't know what to do with Stdlib.succ"
*)

let _ = 
  let module M = struct
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","succ") -> "succ"
    | _ -> id_conv path name
  end
  in
  offshore (module M) .<fun x -> succ @@ x>.
;;
(*
                - : proc_t =
([("x_5", OffshoringIR.TNum I32)], OffshoringIR.TNum I32,
 Block (<abstr>,
  Exp (FunCall (OffshoringIR.OP.Other "succ", [LocalVar "x_5"]))))
*)

let _ = 
  let module M = struct
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","print_endline") -> "print"
    | _ -> id_conv path name
  end
  in
  offshore (module M) .<fun () -> print_endline "hello">.
;;
(*
- : proc_t =
([], OffshoringIR.TVoid,
 Block (<abstr>,
  Exp
   (FunCall (OffshoringIR.OP.Other "print", [Const (Const_string "hello")]))))
*)

let _ = 
  let module M = struct
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","flush_all") -> "fflush"
    | _ -> id_conv path name
  end
  in
  offshore (module M) .<fun () -> flush_all ()>.
;;
(*
- : proc_t =
([], OffshoringIR.TVoid,
 Block (<abstr>, Exp (FunCall (OffshoringIR.OP.Other "fflush", []))))
*)

let _ = offshore (module DefaultConv) 
    .<fun () -> let x = ref 1 in x := !x + 1>.
;;
(*
  - : proc_t =
([], OffshoringIR.TVoid,
 Block (<abstr>,
  Exp
   (FunCall (OffshoringIR.OP.ASSIGN (OffshoringIR.TNum I32),
     [MutVar "x_6";
      FunCall (OffshoringIR.OP.ADD I32,
       [FunCall (OffshoringIR.OP.DEREF (OffshoringIR.TNum I32),
         [MutVar "x_6"]);
        Const (Const_num (I32, "1"))])]))))
*)
let _ = offshore (module DefaultConv) 
    .<fun () -> let x = ref 1 in x := !x + 1; ()>.
;;
(*
  - : proc_t =
([], OffshoringIR.TVoid,
 Block (<abstr>,
  Exp
   (FunCall (OffshoringIR.OP.ASSIGN (OffshoringIR.TNum I32),
     [MutVar "x_7";
      FunCall (OffshoringIR.OP.ADD I32,
       [FunCall (OffshoringIR.OP.DEREF (OffshoringIR.TNum I32),
         [MutVar "x_7"]);
        Const (Const_num (I32, "1"))])]))))
*)

let _ = offshore (module DefaultConv) 
    .<fun () -> let x = ref true in x := false>.
;;
(*
  - : proc_t =
([], OffshoringIR.TVoid,
 Block (<abstr>,
  Exp
   (FunCall (OffshoringIR.OP.ASSIGN OffshoringIR.TBool,
     [MutVar "x_8"; Const (Const_bool false)]))))
*)

(*
let _ = offshore (module DefaultConv) 
    .<fun () -> let x = ref "aa" in x := "bb">.
;;
*)

(*
let _ = 
  try ignore (offshore (module DefaultConv) 
                .<fun () -> let x = ref (ref 1) in x := ref 2>.);
  assert false
  with Failure e -> e
;;
*)

(*
- : string =
"typ_of: the OCaml type int ref ref is (not yet) supported:only base-type ref types are allowed"
*)

let _ = offshore (module DefaultConv) 
                .<fun () -> let x = ref 1 in let y = x in y := 2>.
;;

(*
  - : proc_t =
([], OffshoringIR.TVoid,
 Block (<abstr>,
  Exp
   (FunCall (OffshoringIR.OP.ASSIGN (OffshoringIR.TNum I32),
     [LocalVar "y_10"; Const (Const_num (I32, "2"))]))))
*)

let _ = offshore (module DefaultConv) 
    .<fun a n -> forloop 0 ~upe:n ~step:1 (fun i -> a.(i) <- i)>.
;;
(*
  - : proc_t =
([("a_14", OffshoringIR.TArray1 (OffshoringIR.TNum I32));
  ("n_15", OffshoringIR.TNum I32)],
 OffshoringIR.TVoid,
 Block (<abstr>,
  For
   {OffshoringIR.id = "i_16"; ty = OffshoringIR.TNum I32; guard = None;
    lwb = Const (Const_num (I32, "0")); upe = LocalVar "n_15";
    step = Const (Const_num (I32, "1"));
    body =
     Block (<abstr>,
      Exp
       (FunCall (OffshoringIR.OP.Array1_set (OffshoringIR.TNum I32),
         [LocalVar "a_14"; LocalVar "i_16"; LocalVar "i_16"])))}))
*)

let _ = offshore (module DefaultConv) 
    .<fun a n -> forloop 0 ~upe:n ~step:1 @@ fun i -> a.(i) <- i>.
;;

let _ = offshore (module DefaultConv) 
    .<fun a n -> for i=0 to n-1 do a.(i) <- i done>.;;
;;
(*
  - : proc_t =
([("a_17", OffshoringIR.TArray1 (OffshoringIR.TNum I32));
  ("n_18", OffshoringIR.TNum I32)],
 OffshoringIR.TVoid,
 Block (<abstr>,
  For
   {OffshoringIR.id = "i_19"; ty = OffshoringIR.TNum I32; guard = None;
    lwb = Const (Const_num (I32, "0")); upe = LocalVar "n_18";
    step = Const (Const_num (I32, "1"));
    body =
     Block (<abstr>,
      Exp
       (FunCall (OffshoringIR.OP.Array1_set (OffshoringIR.TNum I32),
         [LocalVar "a_17"; LocalVar "i_19"; LocalVar "i_19"])))}))
*)

  (*
let add_pgm : 
  ((float, float32_elt, c_layout) Array1.t const -> 
   (float, float32_elt, c_layout) Array1.t -> int -> unit) code =
  .<fun a c n ->

    forloop (get_group_id 0) (n / 1024) (get_num_groups 0) @@ fun gid ->
    forloop (get_local_id 0) 256 (get_local_size 0) @@ fun lid ->
    forloop 0 4 1 @@ fun i ->
      let idx = i + 4 * lid + 1024 * gid in
      Array1.set c i @@ Array1.get (unconst a) idx
 >.

let _ = offshore add_pgm
   *)

(* From the GPGPU project *)
let _ = 
  let module M = struct
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Mockgpu",n) -> n                    (* everyting in the GPU namespace *)
    | _             -> id_conv path name
  end
  in
  offshore (module M) 
  .<fun a c n ->
    let open Mockgpu in
    let open Bigarray in
    forloop (get_group_id 0) ~upe:(n / 1024) ~step:(get_num_groups 0) @@ 
    fun gid ->
    forloop (get_local_id 0) ~upe:256 ~step:(get_local_size 0) @@ fun lid ->
    forloop 0 ~upe:4 ~step:1 @@ fun i ->
      let idx = i + 4 * lid + 1024 * gid in
      Array1.set c i @@ (Array1.get a idx) + 1
 >.
;;

let _ = Printf.printf "\nAll Done\n";;
