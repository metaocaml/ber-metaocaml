(* Reifying (and printing) the type of a code expression *)
(* This code illustrates type introspection facilities *)

#directory "../parsing";;
#directory "../typing";;
#directory "../toplevel";;
#directory "../utils";;

(* This must be loaded first! It is stateful, and affects Predef *)
#load "ident.cmo";; 

(* Load the rest of the compiler *)
#load "misc.cmo";;
#load "path.cmo";;
#load "types.cmo";;
#load "btype.cmo";;
#load "tbl.cmo";;
#load "subst.cmo";;
#load "predef.cmo";;
#load "datarepr.cmo";;
#load "config.cmo";;
#load "consistbl.cmo";;
#load "clflags.cmo";;
#load "env.cmo";;
#load "ctype.cmo";;
#load "printast.cmo";;
#load "oprint.cmo";;
#load "primitive.cmo";;
#load "printtyp.cmo";;

open Ident   (* Just to make sure Ident is loaded first! *)
open Types;;
open Typedtree;;
open Format
;;

(* Obtain the information about the type and the typing environment *)
let get_type_env = function
    {Parsetree.pexp_ext = Some tr} ->
	 let t : Typedtree.expression = Obj.obj tr in
	 (t.Typedtree.exp_type, t.Typedtree.exp_env)
  | _ -> failwith "get_type_env"
;;

let describe_decl ppf env = function 
  | (Path.Pident id) as p ->
      begin
	try 
	  let decl = Env.find_type p env in
	  Printtyp.type_declaration id ppf decl
	with Not_found -> fprintf ppf "not found\n"
      end
  | _ -> fprintf ppf "not an ident"
;;

let rec describe_type ppf env ty =
  let ty = Btype.repr ty in
    match ty.desc with
    | Tvar -> fprintf ppf "Tvar\n"
    | Tarrow(l, _, _, _) when not (l = "") ->
	fprintf ppf "Labelled arrow\n"
    | Tarrow(l, ty1, ty2, _) ->
	fprintf ppf "Arrow:\n";
	describe_types ppf env [ty1;ty2]
    | Ttuple tyl ->
	fprintf ppf "Tuple:\n";
	describe_types ppf env tyl
    | Tconstr(p, tyl, abbrev) ->
	fprintf ppf "Constructor:\n";
	Printtyp.path ppf p;
	describe_types ppf env tyl;
	describe_decl ppf env p;
	fprintf ppf "\n"
    | Tvariant row ->
	fprintf ppf "TVariant\n"
    | Tobject (fi, nm) ->
	fprintf ppf "Tobject\n"
    | Tsubst ty ->
        describe_type ppf env ty
    | Tlink _ | Tnil | Tfield _ ->
        failwith "describe_type"
    | Tpoly (ty, []) ->
	describe_type ppf env ty
    | Tpoly (ty, tyl) ->
	fprintf ppf "Tpoly\n"
    | Tunivar ->
	fprintf ppf "Tunivar\n"
and describe_types ppf env tyl =
  List.iter (describe_type ppf env) tyl;
  fprintf ppf "\n"
;;

let do_describe cde =
  let (t,tenv) = get_type_env (Obj.magic cde) in
  describe_type std_formatter tenv t;;
  

(* tests *)

type t1 = A of int | B of bool list;;

let t1exp' () = (failwith "na" : t1);;

do_describe .<t1exp'>.;;

(*
Arrow:
Constructor:
unit
type unit = ()
Constructor:
t1
type t1 =
                                                            A of int
                                                          | B of bool list
*)

type 'a t2 = | A of t1 | B of 'a;;
let t2exp' () = (failwith "na" : 'a t2);;

do_describe .<t2exp'>.;;

(*
Arrow:
Constructor:
unit
type unit = ()
Constructor:
t2Tvar

type 'a t2 =
                                                                 A of t1
                                                               | B of 'a

*)


(*
module Units : sig
     type 'a t
     val to_feet : float -> [`Feet ] t
     val to_meters : float -> [`Meters] t
     val add : 'a t -> 'a t -> 'a t
     val print : 'a t -> unit
end = struct
     type 'a t = float
     let to_feet x=x
     let to_meters x=x
     let add x y = x +. y
     let print (x : 'a t) = Printf.printf "%f (units)" x;
       do_describe .<x>.
end;;
open Units;;

let test_unit = print (add (to_meters 1.) (to_meters 2.));;

module Units1 : sig
     type 'a t
     val to_feet : float -> [`Feet ] t
     val to_meters : float -> [`Meters] t
     val add : 'a t -> 'a t -> 'a t
     val print : 'a t -> unit
end = struct
     type 'a t = float
     let to_feet x=x
     let to_meters x=x
     let add x y = x +. y
     let print (x : 'a t) = Printf.printf "%f (units)" x;
       .! .<do_describe .<x>.>.
end;;
open Units1;;

let printx (x : 'a t) = .! .<do_describe .~(let v = .<x>. in .<v>.)>.;;
let tt = printx (to_meters 1.);;


let test_unit1 = print (add (to_meters 1.) (to_meters 2.));;

let x = to_meters 1. in do_describe .<x>.;;

type 'a ft = int;;
type uf;;
type um;;


let printfx (x : 'a ft) = .! .<do_describe .~(let v = .<x>. in .<[v]>.)>.;;

let printfx (x : 'a ft) = let v = .<[x]>. in .! .<do_describe v>.;;
let tt = printfx (1 : uf ft);;

let printfx (x : 'a ft) = let v1 = .<[x]>. in
   do_describe ((.! v1));;


let v = .<[x]>. in .! .<do_describe v>.;;

*)
