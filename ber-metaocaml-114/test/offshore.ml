(* Tests of offshoring *)

(*
Bigarray.Array2 get and put (say, building a unit matrix or
adding a unit matrix or matrix add)


fun arr1 arr2 ->
  arr1.(1) <- 1;
  arr2.(1) <- 1;
  (if test then arr1 else arr2).(3) <- 3


Include the FILE example into offshoring as the test

 let x = fopen "ddd" in foo x
 let x = fopen "ddd" in foo !x  (fopen returns FILE ref)
 let x = fopen "ddd" in x := NULL
 let x = fopen "ddd" in x := fopen "xxx"
 let x = ref (fopen "ddd") in x := NULL
 let x = ref (fopen "ddd") in x := fopen "xxx"

*)

(*
#load "offshoring.cma";;
*)

open Offshoring

(*
let _  = offshore_to_c ~out:Format.str_formatter ~name:"foo" 
    .<fun x -> x + 1>.;
      Format.flush_str_formatter ()
*)
(*
      - : string = "int foo(int x_1)\n{\n   return (x_1 + 1);}\n"
*)


let _  = offshore_to_c ~name:"foo" 
    .<fun x -> x + 1>.

(*
int foo(int const x_1){
  return (x_1 + 1);
}
*)

(* compare modulo renamed identifiers: that is, strip _[0-9]+ 
   We also convert "( " to "(" 
   (used below to prevent false comments)
*)
let strip_num_suffix : char Seq.t -> char Seq.t = 
  let open Seq in
  let rec loop s  = match uncons s with
  | None -> empty
  | Some ('(',t) -> cons '(' @@ begin
      match uncons t with
      | None -> empty
      | Some (' ',t) -> loop t
      | Some (h,t)   -> cons h t |> loop
  end
  | Some ('_',t) -> begin
      match uncons t with
      | None -> return '_'
      | Some ('0'..'9',t) -> 
          drop_while (function '0'..'9' -> true | _ -> false) t |> loop
      | Some (h,t) -> cons '_' (cons h t |> loop)
  end
  | Some (h,t) -> cons h @@ loop t
  in loop

let test ?cnv exp expect =  
  let res  = offshore_to_c ?cnv ~name:"foo" exp ~out:Format.str_formatter;
  Format.flush_str_formatter () in
  if (Seq.equal (=) (String.to_seq res |> strip_num_suffix)
                        (String.to_seq expect |> strip_num_suffix))
      then () else (print_endline res; 
                    print_endline expect;
                    failwith "differs from the expected")

    

let _ = test .<fun x -> x + 1>.
 {q|
int foo(int const x_1){
  return (x_1 + 1);
}
|q}

let _  = test .<fun x y -> x * y + 1>.
 {q|
int foo(int const x_2,int const y_3){
  return ((x_2 * y_3) + 1);
}
|q}
;;


let _ = 
  try ignore (offshore_to_c ~name:"foo" .<fun x -> succ @@ x>.);
  assert false
  with Failure e -> e
;;
(*
      - : string = "Don't know what to do with Stdlib.succ"
*)

let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","succ") -> "succ"
    | _ -> id_conv path name
  end
  in
  test ~cnv:(module M) .<fun x -> succ @@ succ @@ x>.
 {q|
int foo(int const x_5){
  return succ(succ(x_5));
}
|q}
;;


let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","succ") -> "succ"
    | _ -> id_conv path name
  end
  in
  test ~cnv:(module M) .<fun x -> x |> (-) 3 |> succ >.
 {q|
int foo(int const x_6){
  return succ(3 - x_6);
}
|q}
;;

let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","succ") -> "succ"
    | _ -> id_conv path name
  end
  in
  test ~cnv:(module M) .<fun x -> let () = assert false in x+1>.
 {q|
int foo(int const x_7){
  assert(false);
  return (x_7 + 1);
}
|q}
;;

let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","print_endline") -> "print"
    | _ -> id_conv path name
  end
  in
  test ~cnv:(module M) .<fun () -> print_endline "hello">.
 {q|
void foo(){
  print("hello");
}
|q}
;;

let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","print_endline") -> "print"
    | _ -> id_conv path name
  end
  in
  test ~cnv:(module M) .<fun () -> let () = print_endline "hello" in ()>.
 {q|
void foo(){
  print("hello");
}
|q}
;;

let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","print_endline") -> "print"
    | _ -> id_conv path name
  end
  in
  try ignore (offshore_to_c ~cnv:(module M) ~name:"foo" 
                .<fun () -> let x = print_endline "hello" in x>.);
  assert false
  with Failure e -> e
;;

(*
"Variables of unit types not allowed: \n[\n  structure_item (//toplevel//[11,271+32]..//toplevel//[11,271+33])\n    Tstr_eval\n    expression (//toplevel//[11,271+32]..//toplevel//[11,271+33])\n      Texp_ident \"x_10/1496\"\n]\n\n"
*)

let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Stdlib","flush_all") -> "fflush"
    | _ -> id_conv path name
  end
  in
  test ~cnv:(module M) .<fun () -> flush_all ()>.
 {q|
void foo(){
  fflush();
}
|q}
;;

(* normalization, let-lifting *)

let _ = test .<fun () -> 1>. 
 {q|
int foo(){
  return 1;
}
|q}
;;

(*
let _ = test .<fun () -> let _ = 2 in 1>. 
 {q|
|q}
;;
*)

let _ = test .<fun () -> let x = (let y = 1 in y+1) in x+2>. 
 {q|
int foo(){
  int const y_99 = 1;
  int const x_100 = y_99 + 1;
  return (x_100 + 2);
}
|q}
;;

let _ = test .<fun () -> (let x = 1 in x + 2)+10>. 
 {q|
int foo(){
  int const x_103 = 1;
  return ((x_103 + 2) + 10);
}
|q}
;;

let _ = test .<fun () -> 10 + (let x = 1 in x + 2)>. 
 {q|
int foo(){
  int const x_104 = 1;
  return (10 + (x_104 + 2));
}
|q}
;;

(* Can't lift from the second part of conditional expressions *)
let _ = 
  try ignore (offshore_to_c ~name:"foo" 
    .<fun n -> n > 0 && (let x = n in x > 1)>.);
  assert false
  with Failure e -> e
;;

let _ = 
  try ignore (offshore_to_c ~name:"foo" 
    .<fun n -> if n > 1 then let x = n in x +1 else 3>.);
  assert false
  with Failure e -> e
;;

(*
- : string = "The local binding (to x_112) is not allowed in this context"
*)

let _ = 
  try ignore (offshore_to_c ~name:"foo" 
    .<fun n -> while let x = n in x > 1 do () done>.);
  assert false
  with Failure e -> e
;;

(*
- : string = "The local binding (to x_110) is not allowed in this context"
*)

(* In the above case, we could have lifted the binding, since it is pure.
   But in cases like below, we can't

  while let x = foo () in let y = x + 1 in x > 1 do ... done

  while let x = ref 1 in ... do ... done
*)


(* Not offshorable code *)
let _ = 
  try ignore (offshore_to_c ~name:"foo" 
                .<fun () -> (while true do () done; 1) + 2>.);
  assert false
  with Failure e -> e
;;

(*
          - : string = "not a simple exp"
*)

let _ = 
  try ignore (offshore_to_c ~name:"foo" 
                .<fun n -> (for i = 0 to n do () done; 1) + 2>.);
  assert false
  with Failure e -> e
;;
(*
          - : string = "not a simple exp"
*)

(* Other operations *)

let _ = test .<fun x -> float_of_int x>.
 {q|
double foo(int const x_11){
  return ((double)x_11);
}
|q}
;;

let _ = test .<fun x -> int_of_float x>.
 {q|
int foo(double const x_11){
  return ((int)x_11);
}
|q}
;;

let _ = test .<fun x y -> x land y>.
 {q|
int foo(int const x_13,int const y_14){
  return (x_13 & y_14);
}
|q}
;;

let _ = test .<fun x y -> Int.logand x y>.
 {q|
int foo(int const x_13,int const y_14){
  return (x_13 & y_14);
}
|q}
;;

let _ = test .<fun x y -> Int.logxor x y>.
 {q|
int foo(int const x_13,int const y_14){
  return (x_13 ^ y_14);
}
|q}
;;

let _ = test .<fun x y -> x = y +. 1.>.
 {q|
bool foo(double const x_21,double const y_22){
  return (x_21 == (y_22 + 1.));
}
|q}
;;


(* Mutable variables *)

let _ = test .<fun () -> let x = ref 1 in x := !x + 1>.
 {q|
void foo(){
  int x_8 = 1;
  x_8 = x_8 + 1;
}
|q}
;;

let _ = test .<fun () -> let x = ref 1 in x := !x + 1; ()>.
 {q|
void foo(){
  int x_9 = 1;
  x_9 = x_9 + 1;
}
|q}
;;

let _ = test .<fun () -> let x = ref 1 in let y = !x in y>.
 {q|
int foo(){
  int x_123 = 1;
  int const y_124 = x_123;
  return y_124;
}
|q}
;;

let _ = test .<fun () -> let x = ref true in x := false>.
 {q|
void foo(){
  bool x_11 = true;
  x_11 = false;
}
|q}
;;


(*
let _ = offshore_to_c ~name:"foo"
    .<fun () -> let x = ref "aa" in x := "bb">.
;;
*)

let _ = 
  try
  offshore_to_c ~name:"foo"
                .<fun () -> let x = ref (ref 1) in x := ref 2>.;
  assert false
  with Failure e -> e
;;
(*
          - : string = "ref outside of let x = ref e binders is not supported"
*)

let _ = test .<fun () -> let x = ref 1 in let y = x in y := 2>.
 {q|
void foo(){
  int x_15 = 1;
  int * const y_16 = &x_15;
  ( *y_16) = 2;
}
|q}
;;

let _ = test
    .<fun () -> let x = ref 1 in let y = ref x in !y := 2; !x>.
 {q|
int foo(){
  int x_9 = 1;
  int * y_10 = &x_9;
  ( *y_10) = 2;
  return x_9;
}
|q}
;;

let _ = test 
    .<fun x -> x := !x + 1>.
 {q|
void foo(int * const x_1){
  ( *x_1) = ( *x_1) + 1;
}
|q}
;;

let _ = test 
    .<fun () -> let x = ref 1 in incr x>.
 {q|
void foo(){
  int x_38 = 1;
  x_38++;
}
|q}
;;

let _ = test 
    .<fun () -> let x = ref 1 in decr x>.
 {q|
void foo(){
  int x_38 = 1;
  x_38--;
}
|q}
;;

(* Two tests used in earlier papers *)
let _ = test 
    .<fun y -> let x = ref 0 in x := 1; incr x; !x + y>.
 {q|
int foo(int const y_115){
  int x_116 = 0;
  x_116 = 1;
  x_116++;
  return (x_116 + y_115);
}
|q}
;;

let _ = test 
    .<fun y -> let x = ref 0 in let z = x in z := 42; !x + y>.
 {q|
int foo(int const y_117){
  int x_118 = 0;
  int * const z_119 = &x_118;
  ( *z_119) = 42;
  return (x_118 + y_117);
}
|q}
;;

(* From the reftypes slides and papers *)
let _ = test 
    .<fun () -> let x = ref 0 in float_of_int (!x + 1)>.
 {q|
double foo(){
  int x_3 = 0;
  return ((double)(x_3 + 1));
}
|q}
;;

let _ = test 
    .<fun x -> float_of_int (!x + 1)>.
 {q|
double foo(int * const x_8){
  return ((double)(( *x_8) + 1));
}
|q}
;;

let _ = test 
    .<fun () -> let x = ref 0 in let y = ref x in !(!y)>.
 {q|
int foo(){
  int x_11 = 0;
  int * y_12 = &x_11;
  return ( *y_12);
}
|q}
;;

let _ = test 
    .<fun () -> 
    let x = ref 1 in 
    let z = ref 10 in 
    let y = ref x in 
    ! y :=  41; 
    y := z; 
    ! y := -41; 
    !x + !z >.
 {q|
int foo(){
  int x_13 = 1;
  int z_14 = 10;
  int * y_15 = &x_13;
  ( *y_15) = 41;
  y_15 = &z_14;
  ( *y_15) = -41;
  return (x_13 + z_14);
}
|q}
;;

(* Conditional expressions *)

let _ = test
    .<fun x -> if x > 0 then 1 + x else 3>.
 {q|
int foo(int const x_40){
  return (x_40 > 0 ? 1 + x_40 : 3);
}
|q}
;;

let _ = test
    .<fun x -> x <> 0 && 10 / x > 2>.
 {q|
bool foo(int const x_41){
  return ((x_41 != 0) && ((10 / x_41) > 2));
}
|q}
;;

let _ = test
    .<fun x -> x = 0 || 10 / x > 2>.
 {q|
bool foo(int const x_41){
  return ((x_41 == 0) || ((10 / x_41) > 2));
}
|q}
;;

(* Loops *)

let _ = test
    (let open OffshoringIR in
    .<fun a n -> forloop 0 ~upe:n ~step:1 (fun i -> a.(i) <- i)>.)
 {q|
void foo(int * const a_17,int const n_18){
  for (int i_19 = 0; i_19 < n_18; i_19 += 1)
    (a_17[i_19]) = i_19;
}
|q}
;;


let _ = test
    .<fun a n -> for i=0 to n-1 do a.(i) <- i done>.
 {q|
void foo(int * const a_20,int const n_21){
  for (int i_22 = 0; i_22 < n_21; i_22 += 1)
    (a_20[i_22]) = i_22;
}
|q}
;;

(* From the GPGPU project *)
(*
let _ = 
  let module M = struct
    open OffshoringIR
    include DefaultConv
    let id_conv path name = match (path,name) with
    | ("Mockgpu",n) -> n                    (* everyting in the GPU namespace *)
    | _             -> id_conv path name
  end
  in
  test ~cnv:(module M) 
  .<fun a c n ->
    let open Mockgpu in
    let open Bigarray in
    let open OffshoringIR in
    forloop (get_group_id 0) ~upe:(n / 1024) ~step:(get_num_groups 0) @@ 
    fun gid ->
    forloop (get_local_id 0) ~upe:256 ~step:(get_local_size 0) @@ fun lid ->
    forloop 0 ~upe:4 ~step:1 @@ fun i ->
      let idx = i + 4 * lid + 1024 * gid in
      Array1.set c i @@ (Array1.get a idx) + 1
 >.
 {q|
|q}
;;
*)


let _ = Printf.printf "\nAll Done\n";;
