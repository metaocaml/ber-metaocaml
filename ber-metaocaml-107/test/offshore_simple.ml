(* Simple test of offshoring *)

open OffshoringIR

let _ = offshore (module DefaultConv) .<fun x -> x + 1>.
(*
- : proc_t =
Fun ([("x_1", OffshoringIR.TInt)], OffshoringIR.TInt,
 FunCall ("+", [LocalVar ("x_1", OffshoringIR.TInt); Const <abstr>]))
*)

let _ = offshore (module DefaultConv) .<fun x y -> x * y + 1>.
(*
- : proc_t =
Fun ([("x_2", OffshoringIR.TInt); ("y_3", OffshoringIR.TInt)],
 OffshoringIR.TInt,
 FunCall ("+",
  [FunCall ("*",
    [LocalVar ("x_2", OffshoringIR.TInt);
     LocalVar ("y_3", OffshoringIR.TInt)]);
   Const <abstr>]))
*)

(*
let _ = offshore (module DefaultConv) .<fun x -> succ @@ x>.
Exception: Failure "Don't know what to do with Stdlib.succ".
*)

let _ = 
  let module M = struct
    include DefaultConv
    let id_conv path name = match (path,(name : varname :> string)) with
    | ("Stdlib","succ") -> "succ"
    | _ -> id_conv path name
  end
  in
  offshore (module M) .<fun x -> succ @@ x>.

(*
- : proc_t =
Fun ([("x_3", OffshoringIR.TInt)], OffshoringIR.TInt,
 FunCall ("succ", [LocalVar ("x_3", OffshoringIR.TInt)]))
*)

let _ = offshore (module DefaultConv) 
    .<fun a n -> forloop 0 n 1 (fun i -> a.(i) <- i)>.
(*
  - : proc_t =
Proc
 ([("a_4", OffshoringIR.TArray1 OffshoringIR.TInt);
   ("n_5", OffshoringIR.TInt)],
 For
  {OffshoringIR.id = "i_6"; ty = OffshoringIR.TInt; lwb = Const <abstr>;
   upb = LocalVar ("n_5", OffshoringIR.TInt); step = Const <abstr>;
   body =
    FunCallC ("array1_set",
     [LocalVar ("a_4", OffshoringIR.TArray1 OffshoringIR.TInt);
      LocalVar ("i_6", OffshoringIR.TInt);
      LocalVar ("i_6", OffshoringIR.TInt)])})
*)

let _ = offshore (module DefaultConv) 
    .<fun a n -> for i=0 to n-1 do a.(i) <- i done>.;;

(*
- : proc_t =
Proc
 ([("a_7", OffshoringIR.TArray1 OffshoringIR.TInt);
   ("n_8", OffshoringIR.TInt)],
 For
  {OffshoringIR.id = "i_9"; ty = OffshoringIR.TInt; lwb = Const <abstr>;
   upb = LocalVar ("n_8", OffshoringIR.TInt); step = Const <abstr>;
   body =
    FunCallC ("array1_set",
     [LocalVar ("a_7", OffshoringIR.TArray1 OffshoringIR.TInt);
      LocalVar ("i_9", OffshoringIR.TInt);
      LocalVar ("i_9", OffshoringIR.TInt)])})

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

(* Simple converter from IR to C
   It is oversimplified and is for the simple example only!
   Generally, the convertion to C is more involved.
   But the following converter suffices for our simple tests,
   and gives the idea of the convertion to C

   The function prints the resulting C code
*)

open Format

let rec pr_typ : formatter -> typ -> unit = fun ppf -> function
  | TUnit  -> pp_print_string ppf "void"
  | TInt   -> pp_print_string ppf "int"
  | TFloat -> pp_print_string ppf "float"
  | TArray1 tp -> fprintf ppf "%a *" pr_typ tp
  | _      -> failwith "not yet implemented"

let pr_varname : formatter -> varname -> unit = fun ppf vn ->
  pp_print_string ppf (vn : varname :> string)

let pr_arg : formatter -> (varname * typ) -> unit = fun ppf (name,tp) ->
  fprintf ppf "%a %a" pr_typ tp pr_varname name

let pr_arg_sep : formatter -> unit -> unit = fun ppf () ->
  pp_print_string ppf ","; pp_print_space ppf ()

(* Extract and print local variable declarations *)
let pr_decls : formatter -> cmd -> unit = fun ppf cmd ->
  let pr ty v = fprintf ppf "%a %a;@." pr_typ ty pr_varname v in
  let rec loop = function
    | FunCallC _ -> ()                  (* simplification! *)
    | LetC {id;ty;body;_} -> pr ty id; loop body
    | If _  -> ()                       (* If there are decls, they are local*)
    | For {id; ty;_} -> pr ty id
    | While _ -> ()
    | Seq (e1,e2) -> loop e1; loop e2
  in loop cmd

let rec pr_exp : formatter -> exp -> unit = fun ppf -> function
  | Const (Const_int x) -> pp_print_int ppf x
  | KnownVar x
  | LocalVar (x,_) -> pr_varname ppf x
  | FunCall  (name,args) -> fprintf ppf "(%a)" pr_app (name,args)
  | _ ->  failwith "not yet implemented"
 and
 pr_app : formatter -> (varname * exp list) -> unit = fun ppf (name,args) ->
  match ((name : varname :> string), args) with
  | (op,[e1;e2]) when List.mem op ["+";"-";"*"] ->
      fprintf ppf "%a %s %a" 
        pr_exp e1 op pr_exp e2
  | ("array1_get", [ea;ei]) ->
      fprintf ppf "%a[%a]" pr_exp ea pr_exp ei
  | ("array1_set", [ea;ei;e]) ->
      fprintf ppf "%a[%a] = %a" pr_exp ea pr_exp ei pr_exp e
  | _ -> failwith "not yet implemented"


(* print a block of statements *)
let rec pr_block : formatter -> cmd -> unit = fun ppf cmd ->
  fprintf ppf "{%a@,%a}@." pr_decls cmd pr_cmds cmd
 and pr_cmds : formatter -> cmd -> unit = fun ppf -> function
    | FunCallC (name,args) ->
        fprintf ppf "%a;@." pr_app (name,args) 
    | LetC {id;bind;body;_} -> 
        fprintf ppf "%a = %a;@.%a" pr_varname id pr_exp bind pr_cmds body
    | If(e,ct,None) ->
        fprintf ppf "if(%a)%a" pr_exp e pr_block ct
    | If(e,ct,Some ce) ->
        fprintf ppf "if(%a)%a@.else%a" pr_exp e pr_block ct pr_block ce
    | For {id; lwb; upb; step; body} -> 
        fprintf ppf "for(%a=%a; %a<%a; %a+=%a)%a"
          pr_varname id pr_exp lwb 
          pr_varname id pr_exp upb
          pr_varname id pr_exp step
          pr_block body
    | While (exp,cmd) -> 
        fprintf ppf "while(%a)%a" pr_exp exp pr_block cmd
    | Seq (e1,e2) -> pr_cmds ppf e1; pr_cmds ppf e2


(* The main function for offshoring to C *)
let offshore_to_c : name:string -> 'a code -> unit = fun ~name cde ->
  match offshore (module DefaultConv) cde with
  | Fun _ -> failwith "not yet implemented"
  | Proc (args,body) ->
      fprintf std_formatter 
        "void %s(%a)@, %a@." name 
        (pp_print_list ~pp_sep:pr_arg_sep pr_arg) args
        pr_block body

let () = offshore_to_c "foo" 
    .<fun a n -> for i=0 to n-1 do a.(i) <- i done>.;;

(*
void foo(int * a_11, int n_12)
 {int i_13;
for(i_13=0; i_13<n_12; i_13+=1){
a_11[i_13] = i_13;
}
}

*)

let _ = Printf.printf "\nAll Done\n";;
