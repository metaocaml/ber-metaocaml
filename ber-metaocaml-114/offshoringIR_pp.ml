(* A converter from IR to C (actually, C AST),
  eventually printing the resulting code
  We follow C99 and permit interleaving of declarations and statements.
*)

open OffshoringIR
module C = C_ast                        (* compile with -no-alias-deps *)

(* Mapping of IR types to C types
   If the user has extended the IR type, the user has to extend
   the correspondence by calling cnv_typ_ext
*)
let cnv_typ_ext : (typ -> string) ref =
  ref (fun _ -> failwith "type not yet implemented")

let to_ctype ?(specs=[]) : C.typ -> C.ctype = fun x -> C.{typ=x;specs}

let rec cnv_typ ?(specs=[]) : typ -> C.ctype = function
  | TVoid    -> to_ctype ~specs C.Tvoid
  | TBool    -> to_ctype ~specs C.Tbool
  | TChar    -> to_ctype ~specs C.Tchar
  | TNum I32 -> to_ctype ~specs C.Tint
  | TNum I64 -> to_ctype ~specs C.Tint64
  | TNum F32 -> to_ctype ~specs C.Tfloat
  | TNum F64 -> to_ctype ~specs C.Tdouble
  | TArray1 tp 
  | TRef    tp -> to_ctype ~specs (C.Tptr (cnv_typ tp))
  | x      -> to_ctype ~specs (C.Tnamed (!cnv_typ_ext x))
(*
let pr_varname : formatter -> varname -> unit = fun ppf vn ->
  pp_print_string ppf (vn : varname :> string)
*)


let rec cnv_exp : exp -> C.expression = function
  | Const (Const_bool x)   -> 
      C.(Const (Const_num (if x then "true" else "false")))
  | Const (Const_num (_,x)) -> C.(Const (Const_num x)) 
  | Const (Const_char x)    -> C.(Const (Const_char x)) 
  | Const (Const_string x)  -> C.(Const (Const_string x)) 
  | GlobalVar x
  | LocalVar x -> C.Var (x :> string)
  (* By itself, outside the assign and deref context, MutVar is treated as
     variable address.
     See `Mutable Variables and Reference Types?'
     http://okmij.org/ftp/meta-programming/mutable-var.html
  *)
  | MutVar x -> C.(Unary (ADDROF,Var (x :> string)))
  (* special function applications *)
  | FunCall (OP.DEREF _,[MutVar x])  -> 
      C.Var (x :> string)
  | FunCall  (op,args) -> mk_app_exp op (List.map cnv_exp args)
  | Cond (e,eth,eel) ->
      C.Cond (cnv_exp e, cnv_exps eth, cnv_exps eel)
  | And (e1,e2) -> C.(Binary (AND,cnv_exp e1,cnv_exps e2))
  | Or  (e1,e2) -> C.(Binary (OR,cnv_exp e1,cnv_exps e2))
  | Array _ -> 
      failwith "array expressions are supported only as initializers"
 and
 (* Only function calls: ordinary and assignment to mutables
    (the former is actually handled by mk_app_stmt)
 *)
 cnv_exp_as_stmt : exp -> C.simple_statement = function
   | FunCall (OP.ASSIGN _,[MutVar x;e]) -> 
       C.(BIMOD(ASSIGN,Var (x :> string),cnv_exp e))
   | FunCall (OP.ASSIGN _,[ea;e])  -> 
       C.(BIMOD(ASSIGN,Unary(MEMOF, cnv_exp ea),cnv_exp e))
   | FunCall (OP.INCR _,[MutVar x]) -> 
       C.(UNMOD(POSINCR,Var (x :> string)))
   | FunCall (OP.DECR _,[MutVar x]) -> 
       C.(UNMOD(POSDECR,Var (x :> string)))
   | FunCall (op,args) -> 
        mk_app_stmt op (List.map cnv_exp args)
    | _ -> assert false                 (* should be ruled out by types *)
 and
 cnv_exps : exps -> C.expression = function
   | []  -> assert false
   | [e] -> cnv_exp e
   | es  -> match List.rev es with
     | l::inits -> C.Comma (List.rev inits |> List.map cnv_exp_as_stmt,
                            cnv_exp l)
     | _  -> assert false
 and
 mk_app_exp : OP.t -> C.expression list -> C.expression = fun op args ->
  match (op, args) with
  | (OP.ADD _,[e1;e2])  -> C.(Binary(ADD,e1,e2))
  | (OP.SUB _,[e1;e2])  -> C.(Binary(SUB,e1,e2))
  | (OP.MUL _,[e1;e2])  -> C.(Binary(MUL,e1,e2))
  | (OP.DIV _,[e1;e2])  -> C.(Binary(DIV,e1,e2))
  | (OP.MOD _,[e1;e2])  -> C.(Binary(MOD,e1,e2))
  | (OP.BAND _,[e1;e2]) -> C.(Binary(BAND,e1,e2))
  | (OP.BOR _,[e1;e2])  -> C.(Binary(BOR,e1,e2))
  | (OP.XOR _,[e1;e2])  -> C.(Binary(XOR,e1,e2))
  | (OP.SHL _,[e1;e2])  -> C.(Binary(SHL,e1,e2))
  | (OP.SHR _,[e1;e2])  -> C.(Binary(SHR,e1,e2))
  | (OP.EQ _,[e1;e2])   -> C.(Binary(EQ,e1,e2))
  | (OP.NE _,[e1;e2])   -> C.(Binary(NE,e1,e2))
  | (OP.LT _,[e1;e2])   -> C.(Binary(LT,e1,e2))
  | (OP.LE _,[e1;e2])   -> C.(Binary(LE,e1,e2))
  | (OP.GT _,[e1;e2])   -> C.(Binary(GT,e1,e2))
  | (OP.GE _,[e1;e2])   -> C.(Binary(GE,e1,e2))
  | (OP.NEG _,[e1])     -> C.(Unary(MINUS,e1))
  | (OP.NOT,[e1])       -> C.(Unary(NOT,e1))
  | (OP.BNOT _,[e1])    -> C.(Unary(BNOT,e1))
  | (OP.CAST {onto=tp},[e1])    -> C.(Cast(cnv_typ (TNum tp),e1))
  | (OP.Array1_get _, [ea;ei]) -> C.Index (ea,ei)
  | (OP.REF _, [_]) -> 
      failwith "ref outside of let x = ref e binders is not supported"
  | (OP.DEREF _, [e])   -> C.(Unary(MEMOF,e)) 
  | (OP.Other fn,args)  -> C.(Call(Var (fn :> string),args))
  | _ -> Printf.kprintf failwith "Unsupported op"
 and
 (* general and special applications. Assignment and mutables have
    already been taken care of
 *)
 mk_app_stmt : OP.t -> C.expression list -> C.simple_statement = 
  fun op args ->
  match (op, args) with
  | (OP.Array1_set _, [ea;ei;ev]) ->
      C.(BIMOD(ASSIGN,Index (ea,ei),ev))
   | (OP.Assert,[e]) ->
       C.(CALL (Var "assert",[e]))
  | (OP.Other fn,args)  ->
      C.(CALL(Var (fn :> string),args))
  | _ -> assert false

let cnv_def : binding -> C.let_or_statement = function
  | (None,e) -> C.STMT (cnv_exp_as_stmt e)
  | (Some {id;ty;mut=Cnst},e) ->
      C.(LET (((id :> string),cnv_typ ~specs:[C.S_const] ty),
              Init_single (cnv_exp e)))
  | (Some _,Array _) ->
      failwith "Array definitions are not yet implemented"
  | (Some {id;ty;mut=Mut},e) ->
      C.(LET (((id :> string),cnv_typ ty),Init_single (cnv_exp e)))

(* Convert a block *)
let rec cnv_block (needs_return:bool) : block -> C.block = function
  | Unit when needs_return ->
      failwith "Function returning a value has the empty body"
  | Unit -> []
  | Block (decls,stmt) ->
      Sq.fold_right (fun d s -> C.SIMPLE (cnv_def d) :: s)
                    (cnv_stmt needs_return stmt) decls
 and cnv_stmt (needs_return:bool) : stmt -> C.block = function
  | Exp e when needs_return ->
      C.[RETURN (cnv_exp e)]
  | Seq (e1,e2) -> 
      cnv_block false e1 @ cnv_block needs_return e2
  | _ when needs_return ->
      failwith "Function returning a value does not end in an exp"
  | Exp e  ->
      C.[SIMPLE (STMT (cnv_exp_as_stmt e))]
  | If(e,ct,None) ->
      C.[IF(cnv_exp e, cnv_block false ct,[])]
  | If(e,ct,Some ce) ->
      C.[IF(cnv_exp e, cnv_block false ct,cnv_block false ce)]
  | While (exps,cmd) -> 
      C.[WHILE (cnv_exps exps, cnv_block false cmd)] 
  | For {id; ty; lwb; upe; step; guard; body} -> 
        let v = (id :> string) in
        C.[FOR ( LET((v,cnv_typ ty),Init_single (cnv_exp lwb)),
                 begin match guard with
                 | None ->   Binary(LT,Var v,cnv_exp upe)
                 | Some g -> Binary(AND,Binary(LT,Var v,cnv_exp upe),
                                        cnv_exp g)
                 end,
                 BIMOD(ADD_ASSIGN,Var v,cnv_exp step),
                 cnv_block false body)]

(* Convert the complete procedure. Arguments are constant *)
let cnv_proc  : name:string -> proc_t -> C.declaration = 
 fun ~name (args,ty,body) ->
  C.(FUNDEF (cnv_typ ty, name,
      List.map 
          (fun (v,t) -> 
            ((v : OffshoringIR.varname :> string),cnv_typ ~specs:[S_const] t))
      args,
      cnv_block (ty <> TVoid) body))

(* Pretty-printing to C *)
let pp_to_c : 
 ?out:Format.formatter -> name:string -> proc_t -> unit = 
 fun ?(out=Format.std_formatter) ~name proc ->
   cnv_proc ~name proc |> C_pp.pr_decl out
