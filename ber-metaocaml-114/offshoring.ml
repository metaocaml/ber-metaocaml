(* Offshoring
   Converting a simple imperative subset of MetaOCaml-generated code
   to OffshoringIR, which can then be pretty-printed to C
   or other low-level language

   Simple OCaml code may be considered as C written in different syntax.
   The following helps bring OCaml closer to C.
   For more details, see the paper `Generating C'
   http://okmij.org/ftp/meta-programming/tutorial/genc.html#gen-c

   We handle only an (imperative) subset of OCaml, as typical in offshoring.

   We take advantage of the fact that the our OCaml source is presumed
   to have been generated by MetaOCaml: All identifiers
   have unique names and all types are fully qualified.

   We now normalize the code (to better match C) and detect
   patterns impossible to translate, like
    (while ... do done; 1) + 2

   We now implement the approach to mutable variables described in
   `Do Mutable Variables Have Reference Types?' (ML2022)
   We track the mutability: a variable introduced by
   let x = ref e in ....
   is considered mutable, whose type is the type of e, NOT
   a ref type of e!
   Occurrences of ref e outside the above pattern are not supported
   (the user may translate them to malloc/alloc, with a custom converter)
*)

(* 
#directory "+compiler-libs";;
*)

open OffshoringIR

(* Intermediate expression language, before normalization
   It reflects the simple subset of OCaml that we handle 

   It is a mirror of Typedtree, with easier-to-deal type representation,
   and with a bit of desuraging (of @@ and |>)
   We also desugar let () = e1 in e2 and let _ = e1 in e2 as e1; e2
 *)
type rexp =
  | Const    of constant_t              (* Constant/literal: int, bool,...*)
  | Array    of rexp list               (* immediate array *)
  | LocalVar of varname * typ           (* Locally-bound variable *)
  | KnownVar of OP.t                    (* defined in other modules/Stdlib *)
  | FunCall  of OP.t * rexp list        (* Calls only to known functions *)
  | Let      of {id: varname; ty: typ; bind: rexp; body: rexp;
                 attrs: attribute list}
  | Cond     of rexp * rexp * rexp      (* Conditional expression *)
  | If       of rexp * rexp * rexp option  (* branches: unit type *)
  | Seq      of rexp * rexp
  | For      of {id: varname; ty:typ;
                 lwb: rexp; upe: rexp; step: rexp; body: rexp}
  | While    of rexp * rexp 
  | Unit                                (* empty statement *)

(* User-defined converters-helpers *)
module type converters = sig
  type pathname = string                (* a fully-qualified name *)
  (* Convert a type, with the head constructor of the given qualified
     name and the given parameters
   *)
  val type_conv : pathname -> typ list -> typ
  (* Convert a qualified id, given the module path and the name,
     into something that, say, a C code generator may use for a name.
     Some module names like Opencl may be known to the C generator
     and treated specially.
   *)
  val id_conv : pathname -> string -> string
end

(* Convert an OCaml type to the simpler representation typ,
   raising exception on the types we do not handle
*)

type typ +=
  | TVariable                           (* sometimes inevitably occurs
                                           We do not do any substitutions, etc*)
  | TArrow of typ * typ                 (* Used temporarily for looking up
                                           operation names, which may
                                           depend on types
                                        *)
 			
let typ_of : (module converters) -> Types.type_expr -> Env.t -> typ 
 = fun (module Conv) typ env ->
  let open Types in
  let typ = Ctype.correct_levels typ in
  let rec loop typ =
    let exp_ty = Ctype.expand_head env typ in
    match get_desc exp_ty with
    | Tconstr(p, _, _) when Path.same p Predef.path_unit ->
        TVoid
    | Tconstr(p, _, _) when Path.same p Predef.path_int ->
        TNum I32
    | Tconstr(p, _, _) when Path.same p Predef.path_char ->
        TChar
    | Tconstr(p, _, _) when Path.same p Predef.path_string ->
        TString
    | Tconstr(p, _, _) when Path.same p Predef.path_bool ->
        TBool
    | Tconstr(p, _, _) when Path.same p Predef.path_float ->
        TNum F64
    | Tconstr(p, [t], _) when Path.same p Predef.path_array ->
        TArray1 (loop t)
    | Tconstr(p, args, _) -> loop_constr (Path.name p, args)
    | Tvar _ -> TVariable
    | Tarrow (Asttypes.Nolabel, t1, t2, _) -> TArrow (loop t1, loop t2)
    | _ -> 
       Format.kasprintf failwith
          "typ_of: the OCaml type %a is (not yet) supported"
          Printtyp.type_expr typ
  and loop_constr = function
    | ("Bigarray.Array1.t", [t;_;_]) -> TArray1 (loop t)
    | ("Stdlib__Bigarray.Array1.t", [t;_;_]) -> TArray1 (loop t)
    | ("Stdlib__Bigarray.Array2.t", [t;_;_]) -> TArray2 (loop t)
    | ("Stdlib.ref",[t]) -> TRef (loop t)
    | ("OffshoringIR.float32",[]) -> TNum F32
    | (p,ts) -> Conv.type_conv p (List.map loop ts)

  in loop typ
	  

(* Alas, Printtyped only exports functions for printing structures and
   signatures. If we want to print an expression, we have to wrap it
   into a signature
*)
let print_texp : Format.formatter -> Typedtree.expression -> unit =
  fun ppf exp ->
   let open Typedtree in
   let stru = 
    {str_items =
      [{str_desc = Tstr_eval(exp,[]);
        str_loc  = exp.exp_loc;
        str_env  = exp.exp_env}];
     str_type = [];
     str_final_env = exp.exp_env}
    in
    Printtyped.implementation ppf stru

(* Report an expression we aren't handling *)
let fail_exp : string -> Typedtree.expression -> 'a = fun msg exp ->
  Format.kasprintf failwith "%s: @,%a@." msg print_texp exp

(* Handle the top-level function and determine its arguments and their types *)

let analyze_function: 
    (module converters) ->
    Typedtree.expression -> 
      (varname * typ) list *            (* arguments names and types, may be
                                           empty if the funnction is a thunk
                                         *)
      typ *                             (* the type of the result *)
      Typedtree.expression =            (* the function body *)
  fun conv ->
  let open Typedtree in
  let open Asttypes in
  let rec loop first acc = function       
    | { Typedtree.exp_desc = 
        Texp_function{arg_label=Nolabel;
                      cases=[
                        {c_lhs = 
                         {pat_desc =Tpat_var (x,_); 
                          pat_type=typ; pat_env=env};
                         c_guard = None;
                         c_rhs   = body}]}} -> 
        loop false ((local_name (Ident.name x),typ_of conv typ env)::acc) body
    | { Typedtree.exp_desc = 
        Texp_function{arg_label=Nolabel;
                      cases=[
                        {c_lhs = 
                         {pat_desc =
                          Tpat_construct ({txt=Longident.Lident "()"},_,[],_)};
                         c_guard = None;
                         c_rhs   = body}]}} -> 
        if first then
          loop false [] body
        else failwith "unit argument must be the only argument of a function"
    | { Typedtree.exp_desc = Texp_function _ } as e ->
        fail_exp "the code is not a simple function" e
    | e when first -> fail_exp "the code is not a simple function" e
    | e ->
        let tp = e.exp_type and env = e.exp_env in
        (List.rev acc, typ_of conv tp env, e)
  in loop true []


(* Convert from the full OCaml AST to the simplified subset *)

(* Convert the name of an external identifier (defined in other
   modules or stdlib) into the `standard' form: Op.t
   Invoke the user-supplied converted if needed
*)

let op_of : (module converters) -> typ -> (string * string) -> OP.t = 
  fun (module Conv) typ path -> match (path,typ) with
    | (("Stdlib","+"),_)   -> OP.ADD I32
    | (("Stdlib","-"),_)   -> OP.SUB I32
    | (("Stdlib","*"),_)   -> OP.MUL I32
    | (("Stdlib","/"),_)   -> OP.DIV I32
    | (("Stdlib","mod"),_) -> OP.MOD I32
    | (("Stdlib","+."),_)  -> OP.ADD F64
    | (("Stdlib","-."),_)  -> OP.SUB F64
    | (("Stdlib","*."),_)  -> OP.MUL F64
    | (("Stdlib","/."),_)  -> OP.DIV F64

    | (("Stdlib","&&"),_)  -> OP.AND
    | (("Stdlib","||"),_)  -> OP.OR

    | (("Stdlib","land"),_)       -> OP.BAND I32
    | (("Stdlib","lor"),_)        -> OP.BOR I32
    | (("Stdlib.Int","logand"),_) -> OP.BAND I32
    | (("Stdlib.Int","logor"),_)  -> OP.BOR I32
    | (("Stdlib.Int","logxor"),_) -> OP.XOR I32
    | (("Stdlib.Int","lognot"),_) -> OP.BNOT I32
    | (("Stdlib.Int","shift_left"),_)  -> OP.SHL I32
    | (("Stdlib.Int","shift_right"),_) -> OP.SHR I32

    | (("Stdlib","~-"),_)   -> OP.NEG I32
    | (("Stdlib","~-."),_)  -> OP.NEG F64

    | (("Stdlib","="),(TArrow (TNum t,_)))  -> OP.EQ t
    | (("Stdlib","<>"),(TArrow (TNum t,_))) -> OP.NE t
    | (("Stdlib","<"),(TArrow (TNum t,_)))  -> OP.LT t
    | (("Stdlib","<="),(TArrow (TNum t,_))) -> OP.LE t
    | (("Stdlib",">"),(TArrow (TNum t,_)))  -> OP.GT t
    | (("Stdlib",">="),(TArrow (TNum t,_))) -> OP.GE t

    | (("Stdlib","float_of_int"),_) -> OP.CAST {from=I32; onto=F64}
    | (("Stdlib","int_of_float"),_) -> OP.CAST {from=F64; onto=I32}
    | (("Stdlib","truncate"),_)     -> OP.CAST {from=F64; onto=I32}

    | (("Stdlib",":="),(TArrow (TRef t,_))) -> OP.ASSIGN t
    | (("Stdlib","!"),(TArrow (TRef t,_)))  -> OP.DEREF t
    | (("Stdlib","ref"),(TArrow (t,_)))     -> OP.REF t
    | (("Stdlib","incr"),_)     -> OP.INCR I32
    | (("Stdlib","decr"),_)     -> OP.DECR I32

 (*
    | (("Stdlib.Array","make"),_)-> OP.REF t
 *)
    | (("Stdlib.Array","get"),TArrow (TArray1 t,_)) -> OP.Array1_get t
    | (("Stdlib.Array","set"),TArrow (TArray1 t,_)) -> OP.Array1_set t
    | (("Stdlib.Bigarray.Array1","get"),TArrow (TArray1 t,_)) -> OP.Array1_get t
    | (("Stdlib.Bigarray.Array1","set"),TArrow (TArray1 t,_)) -> OP.Array1_set t

    (* Ask the programmer *)  
    | ((p,x), _) -> Conv.id_conv p x |> OP.name

(* Maintaining the precision when printing float
   See the comments in trx.ml for the function float_constant there

let float_constant : float -> string = fun x ->
      if Float.is_integer x then string_of_float x else
      Printf.sprintf "%.17g" x
*)

(*
Codelib.close_code .<1 |> succ>. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code .<1 |> (+) 2 |> succ >. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code .<let _ = 2 in 1>. |> Runcode.typecheck_code;;

Codelib.close_code .<let () = assert false in 1>. |> Runcode.typecheck_code;;

Codelib.close_code .<true && false || true>. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code .<- (1+2)>. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code .<1 land 2>. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code .<forloop 1 ~upe:3 ~step:1 (fun i -> print_int i)>. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code .<forloop 1 ~step:1 ~upe:3 (fun i -> print_int i)>. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code .<forloop 1 ~step:1 ~upe:3 @@ fun i -> print_int i>. |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

Codelib.close_code 
 .<fun a n -> forloop 0 ~upe:n ~step:1 @@ fun i -> a.(i) <- i>.
  |> Runcode.typecheck_code |> print_texp Format.std_formatter;;

*)


let rec rexp_of : (module converters) -> Typedtree.expression -> rexp = 
 fun conv e ->
  let open Typedtree in 
  let open Asttypes in 
  let tp = e.exp_type and env = e.exp_env in
  match e.exp_desc with
  | Texp_ident (p,_,_) -> begin
      let typ = typ_of conv tp env in
      if typ = TVoid then 
         fail_exp "Variables of unit types not allowed" e
      else
      match p with
      | Path.Pident x   -> LocalVar (local_name (Ident.name x),typ)
      | Path.Pdot (p,s) -> KnownVar (op_of conv typ (Path.name p,s))
      | _               -> assert false
      end
  | Texp_constant Asttypes.(Const_int x)   -> 
      Const (Const_num (I32, string_of_int x))
  | Texp_constant Asttypes.(Const_char x)  -> Const (Const_char x)
  | Texp_constant Asttypes.(Const_float x) -> 
      Const (Const_num (F64, x))
  | Texp_constant Asttypes.(Const_string (x,_,_)) -> Const (Const_string x)

  (* Get rid of @@ and replace with the ordinary application *)
  | Texp_apply ({exp_desc = Texp_ident (p,_,_)},
                [(Nolabel, Some e1);e2]) when Path.name p = "Stdlib.@@" ->
    begin
      match e1 with
      | {exp_desc = Texp_apply (e11,args)} ->
           rexp_of conv {e with exp_desc = Texp_apply (e11,args @ [e2])}
      | _ -> rexp_of conv {e with exp_desc = Texp_apply (e1,[e2])}
    end

  (* Get rid of |> and replace with the ordinary application *)
  | Texp_apply ({exp_desc = Texp_ident (p,_,_)},
                [e1;(Nolabel, Some e2)]) when Path.name p = "Stdlib.|>" ->
    begin
      match e2 with
      | {exp_desc = Texp_apply (e21,args)} ->
           rexp_of conv {e with exp_desc = Texp_apply (e21,args @ [e1])}
      | _ -> rexp_of conv {e with exp_desc = Texp_apply (e2,[e1])}
    end

  (* It seems that OCaml nowadays inlines @@. However, it can produce
     nested applications, especially in case
     forloop 1 2 3 @@ fun i -> ...
     If we detect a nested application, make it saturated as much as
     possible
  *)
  | Texp_apply ({exp_desc = Texp_apply (e, args)}, args') ->
    rexp_of conv {e with exp_desc = Texp_apply (e,args @ args')}

  (* Special applications *)
  (* match e for forloop directly! *)
  | Texp_apply ({exp_desc = Texp_ident (p,_,_)},
                [(Nolabel,Some lwb);
                 (Labelled "upe",Some upe);
                 (Labelled "step",Some step);
                 (Nolabel,Some {exp_desc = 
                  Texp_function 
                       {arg_label=Nolabel;
                        cases = [
                         {c_lhs = 
                           {pat_desc =Tpat_var (x,_);
                            pat_env;pat_type};
                          c_guard = None;
                          c_rhs   = body}]}})])
    when
      Path.name p = "OffshoringIR.forloop" ->
        For {id=local_name (Ident.name x); ty=typ_of conv pat_type pat_env;
             lwb=rexp_of conv lwb;
             upe=rexp_of conv upe;
             step=rexp_of conv step;
             body=rexp_of conv body}

  (* General application *)
  | Texp_apply (eo,args) ->
      let op = match rexp_of conv eo with
      | KnownVar x -> x
      | _  -> fail_exp "Only calls to global functions are allowed" e
      in
      let args = List.map (function 
        | (Nolabel,Some e) -> rexp_of conv e
        | _ -> fail_exp "unsupported labeled or default args" e) args in
      let args = 
        if args = [(Unit:rexp)] then [] else
        (if List.exists ((=) (Unit:rexp)) args then
          fail_exp "unit argument in application should be the sole arg" e;
        args)
      in
      FunCall (op,args)

  | Texp_assert e ->
      FunCall (OP.Assert,[rexp_of conv e])

  | Texp_let (Nonrecursive,
               [{vb_pat=
                 {pat_desc=Tpat_var (x,_); pat_type; pat_env};
                 vb_expr}],body)
    -> let ty=typ_of conv pat_type pat_env in
       let bind=rexp_of conv vb_expr in
       Let {id=local_name (Ident.name x); ty; bind; 
            body=rexp_of conv body; attrs=[]}
  | Texp_let (Nonrecursive,
               [{vb_pat= {pat_desc=Tpat_any};
                 vb_expr}],body) ->
      Seq (rexp_of conv vb_expr, rexp_of conv body)
  (* let () = e1 in e2 
     Cannot pattern-match on Tpat_value since the type of its argument
     is private
   *)
  | Texp_match (e1,
                [{c_lhs={pat_desc=Tpat_value _; pat_type; pat_env}; 
                  c_guard=None; c_rhs=e2}], Total) 
      when typ_of conv pat_type pat_env = TVoid ->
      Seq (rexp_of conv e1, rexp_of conv e2)

  | Texp_ifthenelse (etest,eth,Some eel) when typ_of conv tp env <> TVoid ->
      Cond (rexp_of conv etest, rexp_of conv eth, rexp_of conv eel)
  | Texp_ifthenelse (etest,eth,None) ->
      If (rexp_of conv etest, rexp_of conv eth, None)
  | Texp_ifthenelse (etest,eth,Some eel) ->
      If (rexp_of conv etest, rexp_of conv eth, Some (rexp_of conv eel))
  | Texp_sequence(e1,e2) ->
      Seq (rexp_of conv e1, rexp_of conv e2)
  | Texp_array xs -> Array (List.map (rexp_of conv) xs)
  | Texp_construct({txt=Longident.Lident "true"},_,_) ->
      Const (Const_bool true)
  | Texp_construct({txt=Longident.Lident "false"},_,_) ->
      Const (Const_bool false)
  | Texp_construct ({txt=Longident.Lident "()"},_,[]) -> Unit
  | Texp_while (et,body) ->
      While (rexp_of conv et, rexp_of conv body)
  | Texp_for (x,_,elow,ehigh,Upto,body) ->
      let one = Const (Const_num (I32,"1")) in
      (* Make the upper bound exclusive *)
      let upe = match rexp_of conv ehigh with
      | FunCall (OP.SUB _, [x;one']) when one'=one -> x
      | x -> FunCall (OP.ADD I32,[one;x]) in
      For {id=local_name (Ident.name x); ty=TNum I32;
             lwb=rexp_of conv elow;
             upe;
             step=one;
             body=rexp_of conv body}
  | _ -> fail_exp "unhandled expression" e

(* Default conversion module. The programmer is supposed to
   include it and override what is needed
*)
module DefaultConv : converters = struct
  type pathname = string                (* a fully-qualified name *)
  let type_conv : pathname -> typ list -> typ = fun path _args ->
    failwith @@ "Unknown type: " ^ path
  (* Convert a qualified id, given the module path and the name,
     into something that, say, a C code generator may use for a name.
     Some module names like Opencl may be known to the C generator
     and treated specially.
   *)
  let id_conv : pathname -> string -> string = fun path name ->
    failwith @@ "Don't know what to do with " ^ path ^ "." ^ name
end

(*
Codelib.close_code .<let _ = 2 in 1>. |> Runcode.typecheck_code  |> rexp_of (module DefaultConv) ;;

Codelib.close_code .<let () = assert false in 1>. |> Runcode.typecheck_code |> rexp_of (module DefaultConv) ;;

module CNV1 = struct
    include DefaultConv
    let id_conv path name = match (path,(name : varname :> string)) with
    | ("Stdlib","succ") -> "succ"
    | _ -> id_conv path name
end;;

Codelib.close_code .<1 |> succ>. |> Runcode.typecheck_code |> rexp_of (module CNV1) ;;


Codelib.close_code .<1 |> (+) 2 |> succ >. |> Runcode.typecheck_code |> rexp_of (module CNV1) ;;

*)

(* Normalization *)
(* Currently, we reject the code like
   while let x = 1 in x < 1 do ... done
  where the test expression has local variables.
  In the future, we may allow it. We'll have to lift the bindings up:
  but taking care converting the bindings that refer to mutable variables
  (that is, potentially side-effecting expressions). For example, we would
  convert
   while let x = 1 in let y = x + !z in y < 1 do ... done
  to
   let x = 1 in      pure bindings may be lifted as such
   let y = ref uninit in
   while (y := x + !z; y<1) do ... done

The following may be useful, when we get around to implementing such
lifting.
(* Check if an expression is constant 
   Constant expressions are freely movable, copyable
*)
let rec constant_exp : exp -> bool = function
  | Const _ -> true
  | ArrayE es -> List.for_all constant_exp es
  | FunCall (name,es) when List.mem (name :> string) ["+";"-"] ->
      (* XXX add more pure fn *)
      List.for_all constant_exp es
  | _ -> false

(* A pure expression has no side effects.
   Calls to functions may have side effects, unless we know for sure
   the function is pure
   Pure expression may dereference variables. So it is not freely
   movable
 *)
let rec pure_exp : exp -> bool = function
  | Const _ 
  | LocalVar _
  | KnownVar _ -> true
  | ArrayE es -> List.for_all pure_exp es
  | FunCall (name,es) when List.mem (name :> string) ["+";"-";"ref"] ->
      (* XXX add more pure fn *)
      List.for_all pure_exp es
  | _ -> false
*)

let rec block_of : varname list -> rexp -> block = fun mutvars -> function
  | Unit       -> Unit
  | Const c    -> of_exp (Const c)
  | KnownVar (OP.Other v) -> of_exp (GlobalVar v)
  | KnownVar _ -> failwith "Globalvar is an operator"
  | LocalVar (v,_) -> of_exp @@
            if List.mem v mutvars then MutVar v else OffshoringIR.LocalVar v
  | FunCall (OP.AND,[e1;e2]) -> 
      (* Bindings from e1 can be lifted, but not from e2 *)
      let (bindings,e1) = exp_of mutvars e1 in
      let e2 = exp_of mutvars e2 |> to_exps in
      of_exp ~bindings (And (e1,e2))
  | FunCall (OP.AND,_) -> failwith "Unsaturated (&&) application"
  | FunCall (OP.OR,[e1;e2]) -> 
      (* Bindings from e1 can be lifted, but not from e2 *)
      let (bindings,e1) = exp_of mutvars e1 in
      let e2 = exp_of mutvars e2 |> to_exps in
      of_exp ~bindings (Or (e1,e2))
  | FunCall (OP.OR,_) -> failwith "Unsaturated (||) application"
  | FunCall (op,[])   -> of_exp (FunCall (op,[]))
  | FunCall (op,args) ->
      let (bs,args) = List.map (exp_of mutvars) args |> List.split in
      let bindings  = Sq.concat bs in
      of_exp ~bindings (FunCall (op,args))
  | Array args ->
      let (bs,args) = List.map (exp_of mutvars) args |> List.split in
      let bindings  = Sq.concat bs in
      of_exp ~bindings (Array args)
  | Cond (test,th,el) ->
       let (bindings,et) = exp_of mutvars test in
       of_exp ~bindings (Cond (et, exp_of mutvars th |> to_exps,
                                   exp_of mutvars el |> to_exps))
  | If (test,th,el) ->
       let (bindings,et) = exp_of mutvars test in
       Block (bindings,If (et, block_of mutvars th,
                           Option.map (block_of mutvars) el))
  | While (test,body) ->
      let e = exp_of mutvars test |> to_exps in
      Block (Sq.empty,While (e,block_of mutvars body))
  | For {id; ty; lwb; upe; step; body} ->
      let (lb, lwb)  = exp_of mutvars lwb in
      let (ub, upe)  = exp_of mutvars upe in
      let (sb, step) = exp_of mutvars step in
      let body = block_of mutvars body in
      Block (Sq.(lb @ ub @ sb), For {id;ty;lwb;upe;step;body;guard=None})
  | Let {id; ty; bind; body; attrs} ->
      let (bindings, be) = exp_of mutvars bind in
      let (ty,be,mut) = match be with
       | FunCall (OP.REF ty,[e]) -> (ty,e,Mut)
       | _                       -> (ty,be,Cnst)
      in
      let mutvars = if mut = Mut then id::mutvars else mutvars in
      begin match block_of mutvars body with
      (* No need to bind if the body is empty *)
      | Unit -> Block (bindings,Exp be)
      | Block (b,s) ->
       Sq.(Block (bindings @ one (Some {id;ty;mut;attrs},be) @ b, s))
      end

  | Seq (e1,e2) -> seq (block_of mutvars e1) (block_of mutvars e2)
 and exp_of : varname list -> rexp -> (binding Sq.t * exp) = fun mutvars rexp ->
   match block_of mutvars rexp with
   | Unit             -> assert false (* types should have prevented this *)
   | Block (bindings,Exp e) -> (bindings,e)
   | _                      -> failwith "not a simple exp"
 (* See if the bindings can be converted to a list of exp *)
 and to_exps : (binding Sq.t * exp) -> exps = fun (bs,e) ->
   let folder x z = match x with
   | (None,e)      -> e :: z
   | (Some {id},_) -> Printf.kprintf failwith
      "The local binding (to %s) is not allowed in this context" (id :> string)
   in Sq.fold_right folder [e] bs

(* Main offshoring function: convert from the code in the supported 
   subset of OCaml to the intermediate language.
*)

let offshore : (module converters) -> 'a Codelib.code -> proc_t = 
 fun conv cde ->
  let (args,typ,body) = 
    Codelib.close_code cde |> Runcode.typecheck_code |> analyze_function conv
  in (args, typ, rexp_of conv body |> block_of [])

(* The main function for offshoring to C *)
let offshore_to_c : 
 ?cnv:(module converters) -> 
 ?out:Format.formatter -> name:string -> 'a Trx.code -> unit = 
 fun ?(cnv=(module DefaultConv:converters)) 
     ?(out=Format.std_formatter) ~name cde ->
   offshore cnv cde |> OffshoringIR_pp.pp_to_c ~name ~out

