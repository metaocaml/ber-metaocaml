(**			Generic print


If this ever gets integrated with MetaOCaml, the simplest implementation
will be to change the AST printing: when is about to print CSP,
check the type...

$Id: gprint.ml,v 1.3 2006/04/17 01:46:50 oleg Exp $
*)


open Ident   (* Just to make sure Ident is loaded first! *)


(* Given an expression like .<something>., extract the parse tree
   corresponding to something.
   Recall that an object of type code is actually a parsetree
*)

let get_elem_parsetree (x : ('a,'b) code) : Parsetree.expression =
  Obj.magic x
;;

(* Just a helper, for debugging purposes *)
let type_to_str (x : Typedtree.expression) = 
  Printtyp.type_expr Format.str_formatter (x.Typedtree.exp_type);
         Format.flush_str_formatter ();;

(* First, we do a consistency check: Env.initial is the same as
   the initial environment that corresponds to the host system
*)

(*
let initial_env_ids = 
  Predef.build_initial_env 
    (fun iden _ lst -> Ident.name iden :: lst)
    (fun iden _ lst -> Ident.name iden :: lst)
    []
*)


(* Detailed printing function (unlike Path.name), showing timestamps, etc.*)
let string_of_ident i = 
  Ident.print Format.str_formatter i;
  Format.flush_str_formatter ()


let rec string_of_path p = 
  let fmt = Format.str_formatter in
  let rec loop = function
  | Path.Pident i -> Ident.print fmt i
  | Path.Pdot (p,s,fl) -> 
      loop p; Format.fprintf fmt ".%s(%d)" s fl
  | Path.Papply _ -> Format.fprintf fmt "Papply"
  in loop p; 
  Format.flush_str_formatter ()
;;

(* Get the env associated with the host compiler: true env *)
(* This is somewhat silly; still, it's better to be safe than sorry.
   The inconsistency did cause me a lot of trouble once.
*)
let true_env = 
  match get_elem_parsetree .<[None]>. with
    {Parsetree.pexp_ext = Some tr} ->
	 let t : Typedtree.expression = Obj.obj tr in
	 t.Typedtree.exp_env
  | _ -> assert false
;;


let () =
  let itrue =  (* what the type "int" is in the true env *)
    string_of_path 
      (fst (Env.lookup_type (Longident.Lident "int") true_env)) in
  let iint =   (* what it is in the Env.initial *)
    string_of_path
      (fst (Env.lookup_type (Longident.Lident "int") Env.initial)) in
  if itrue = iint then ()
  else failwith ("Inconsistent Env: " ^ itrue ^ " vs. " ^ iint ^
                 "Ensure that the module Ident is loaded first!")
;;
							   

(**    The printing itself *)

(* EvalPath is used only to print exceptions. For now, we force
   Genprintval to make a ``feeble attempt''
*)
module EvalPath = struct
  type value = Obj.t
  exception Error
  let eval_path p = raise Error
  let same_value v1 v2 = (v1 == v2)
end

module Printer = Genprintval.Make(Obj)(EvalPath)
;;

let max_printer_depth = ref 100
let max_printer_steps = ref 300

(* Before attempting to print the value, check to make sure
   that Config.load_path is set. Otherwise,
   the printer could not find .cmi files with the 
   needed type declarations.
*)

let print_value env obj ty =
  if !Config.load_path = [] then Compile.init_path();
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty
;;


(* Analyze the parse tree. If it describes a CSP value, extract the type
   and use print_value. Otherwise, use printAST
*)

let print_parsetree fmt = function
    {Parsetree.pexp_desc = 
       Parsetree.Pexp_cspval (v, _);
       Parsetree.pexp_ext = Some tr} ->
	 let t : Typedtree.expression = Obj.obj tr in
         let () = !Oprint.out_value fmt
                  (print_value t.Typedtree.exp_env v t.Typedtree.exp_type) in
         type_to_str t
   | pt -> Print_code.inpc fmt pt; "<manifest>"
;;


let fprint fmt (x : ('a,'b) code) = 
   print_parsetree fmt (get_elem_parsetree x)
;;

let print (x : ('a,'b) code) = 
   fprint Format.std_formatter x
;;

