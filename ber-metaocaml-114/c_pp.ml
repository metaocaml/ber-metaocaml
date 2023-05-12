(* Pretty-printer of C AST
 *)

open C_ast
open Format

let rec pr_typ : formatter -> typ -> unit = fun ppf -> function
  | Tvoid           -> pp_print_string ppf "void"
  | Tbool           -> pp_print_string ppf "bool" (* needs <stdbool.h> *)
  | Tchar           -> pp_print_string ppf "char"
  | Tshort          -> pp_print_string ppf "short"
  | Tint            -> pp_print_string ppf "int"
  | Tlong           -> pp_print_string ppf "long"
  | Tint64          -> pp_print_string ppf "int64_t" (* needs <stdint.h> *)
  | Tuint           -> pp_print_string ppf "unsigned int"
  | Tulong          -> pp_print_string ppf "unsigned long"
  | Tfloat          -> pp_print_string ppf "float"
  | Tdouble         -> pp_print_string ppf "double"
  | Tnamed str      -> pp_print_string ppf str
  | Tptr ctp        -> fprintf ppf "%a *" pr_ctype ctp
  | Tarray (n,ctp)  -> fprintf ppf "%a [%d]" pr_ctype ctp n
 and pr_ctype : formatter -> ctype -> unit = fun ppf {specs;typ} ->
      fprintf ppf "%a%a%a" pr_specs_storage specs pr_typ typ pr_quals specs
 (* They are always leading, so leave the trailing space *)
 and pr_specs_storage : formatter -> spec list -> unit = fun ppf ->
     List.iter @@ function 
       | S_static        -> pp_print_string ppf "static "
       | S_extern        -> pp_print_string ppf "extern "
       | S_inline        -> pp_print_string ppf "inline "
       | _               -> ()
 (* They are always trailing, so have the leading space 
   According to C spec, the qualifiers should really be trailing
  *)
 and pr_quals : formatter -> spec list -> unit = fun ppf ->
     List.iter @@ function 
       | S_const    -> pp_print_string ppf " const"
       | S_volatile -> pp_print_string ppf " volatile"
       | S_restrict -> pp_print_string ppf " restrict"
       | _          -> ()

let pr_const : formatter -> constant -> unit = fun ppf -> function
  | Const_num str    -> pp_print_string ppf str
  | Const_char '\"'  -> pp_print_string ppf "'\"'"
  | Const_char c     -> fprintf ppf "'%s'" Char.(escaped c)
  | Const_string str -> fprintf ppf "\"%s\"" String.(escaped str)

let pr_unop : formatter -> unary_operator -> unit = fun ppf -> function
  | MINUS  -> pp_print_string ppf "-"
  | PLUS   -> pp_print_string ppf "+"
  | NOT    -> pp_print_string ppf "!"
  | BNOT   -> pp_print_string ppf "~"
  | MEMOF  -> pp_print_string ppf "*"
  | ADDROF -> pp_print_string ppf "&"

(* printed representations and precedences *)
let binops : (binary_operator * (string * int)) list = [
  (MUL, ("*", 12));
  (DIV, ("/", 12));
  (MOD, ("%", 12));
  (ADD, ("+", 11));
  (SUB, ("-", 11));
  (SHL, ("<<", 10));
  (SHR, (">>", 10));
  (LT, ("<", 9));
  (LE, ("<=", 9));
  (GT, (">", 9));
  (GE, (">=", 9));
  (EQ, ("==", 8));
  (NE, ("!=", 8));
  (BAND, ("&", 7));
  (XOR, ("^", 6));
  (BOR, ("|", 5));
  (AND, ("&&", 4));
  (OR, ("||", 3));
 ]

(* We don't do precedence pretty-printing for now *)
let pr_binop : formatter -> binary_operator -> unit = fun ppf op ->
  pp_print_string ppf (List.assoc op binops |> fst)


let binmodops : (binary_modifier * string) list = [
  (ASSIGN, "=");
  (ADD_ASSIGN,  "+=");
  (SUB_ASSIGN,  "-=");
  (MUL_ASSIGN,  "*=");
  (DIV_ASSIGN,  "/=");
  (MOD_ASSIGN,  "%=");
  (BAND_ASSIGN, "&=");
  (BOR_ASSIGN,  "|=");
  (XOR_ASSIGN,  "^=");
  (SHL_ASSIGN,  "<<=");
  (SHR_ASSIGN,  ">>=");
 ]

let pr_assoc : ('a * string) list -> formatter -> 'a -> unit = fun lst ppf op ->
  pp_print_string ppf (List.assoc op lst)

(* Separators for pp_print_list *)
let pp_sep_comma : formatter -> unit -> unit = fun ppf () ->
  pp_print_string ppf ","; pp_print_cut ppf ()

let pp_sep_semi : formatter -> unit -> unit = fun ppf () ->
  pp_print_string ppf ";"; pp_print_space ppf ()

let rec pr_exp : formatter -> expression -> unit = fun ppf -> function
  | Nothing  -> ()
  | Const c  -> pr_const ppf c
  | Var v    -> pp_print_string ppf v
  | Unary (op,Var v) -> fprintf ppf "%a%s" pr_unop op v
  | Unary (op,exp)   -> fprintf ppf "%a(%a)" pr_unop op pr_exp exp
  | Label_addr l     -> fprintf ppf "&&%s" l
  | Binary (op,e1,e2) -> fprintf ppf "@[%a@ %a %a@]"
        pr_paren_exp e1 pr_binop op pr_paren_exp e2
  | Cond(e1,e2,e3)   -> fprintf ppf "(%a ? %a : %a)" 
        pr_exp e1 pr_exp e2 pr_exp e3
  | Cast (ctp,exp)   -> fprintf ppf "(%a)%a" pr_ctype ctp pr_paren_exp exp
  | Call(ef,args)    -> fprintf ppf "%a(%a)" pr_paren_exp ef
        (pp_print_list ~pp_sep:pp_sep_comma pr_exp) args
  | Comma ([],exp)   -> pr_exp ppf exp
  | Comma (sts,exp)  -> fprintf ppf "(%a,%a)"
        (pp_print_list ~pp_sep:pp_sep_comma pr_stmt_simple) sts
        pr_exp exp
  | Index (e1,e2)       -> fprintf ppf "%a[%a]" pr_paren_exp e1 pr_exp e2
  | Memberof (e,fn)     -> fprintf ppf "%a.%s"  pr_paren_exp e fn
  | Memberofptr (e,fn)  -> fprintf ppf "%a->%s" pr_paren_exp e fn
 and pr_paren_exp : formatter -> expression -> unit = fun ppf -> function
   | Const _ | Var _ | Cond _ | Call _ as e -> pr_exp ppf e
   | e -> fprintf ppf "@[(%a)@]" pr_exp e
 (* many statements like if (e) ... contain expressions in parens *)
 and pr_always_paren_exp : formatter -> expression -> unit = fun ppf e ->
   fprintf ppf "@[<2>(%a)@]" pr_exp e

 and pr_typedname : formatter -> typedname -> unit = fun ppf -> function
  | (v,{typ=Tarray(n,ct);specs}) ->
      fprintf ppf "%a%a%a %s[%d]" 
        pr_specs_storage specs pr_ctype ct pr_quals specs v n
  | (v,ct) -> fprintf ppf "%a %s" pr_ctype ct v

 (* Prints trailing semi-colon *)
 and pr_defn : formatter -> definition -> unit = fun ppf (tn,init) ->
  pr_typedname ppf tn;
  match init with
  | Init_none -> fprintf ppf ";"
  | Init_single e -> fprintf ppf "@[<2> =@ %a;@]" pr_exp e
  | Init_many es  -> fprintf ppf " = {@[<2>%a@]};"
        (pp_print_list ~pp_sep:pp_sep_comma pr_exp) es

 (* Does NOT print any terminating character *)
 and pr_stmt_simple : formatter -> simple_statement -> unit = 
  fun ppf -> function
  | NOP              -> ()
  | CALL(ef,args)    -> fprintf ppf "%a@[<2>(%a)@]" pr_paren_exp ef
        (pp_print_list ~pp_sep:pp_sep_comma pr_exp) args
  | UNMOD(POSINCR,e) -> fprintf ppf "%a++" pr_paren_exp e
  | UNMOD(POSDECR,e) -> fprintf ppf "%a--" pr_paren_exp e
  | BIMOD (op,e1,e2) -> fprintf ppf "@[%a %a@ %a@]"
        pr_paren_exp e1 (pr_assoc binmodops) op pr_exp e2

 (* Prints trailing semi-colon *)
 and pr_los : formatter -> let_or_statement -> unit = fun ppf -> function
  | LET d  -> pr_defn ppf d 
  | STMT s -> fprintf ppf "%a;" pr_stmt_simple s

 (* Prints trailing semi-colon, except for blocks *)
 and pr_stmt : formatter -> statement -> unit = fun ppf -> function
  | SIMPLE s  -> pr_los ppf s
  | BLOCK []  -> ()
  | BLOCK [s] -> pr_stmt ppf s
  | BLOCK b   -> pr_block ppf b
  | IF(e,b1,[]) -> fprintf ppf "if %a@,%a" pr_always_paren_exp e pr_subbl b1
  | IF(e,b1,b2) -> fprintf ppf "if %a@,%a@ else %a"
        pr_always_paren_exp e pr_subbl b1 pr_subbl b2
  | WHILE(e,b)  -> fprintf ppf "while %a@,%a" 
        pr_always_paren_exp e pr_subbl b
  | DOWHILE(e,b) -> fprintf ppf "do@,%a@,while %a;"
        pr_subbl b pr_always_paren_exp e
  | FOR(sinit,e,sincr,b) -> fprintf ppf "for @[<2>(%a@ %a;@ %a)@]%a"
        pr_los sinit pr_exp e pr_stmt_simple sincr pr_subbl b
  | BREAK          -> fprintf ppf "break;"
  | CONTINUE       -> fprintf ppf "continue;"
  | RETURN Nothing -> fprintf ppf "return;"
  | RETURN e       -> fprintf ppf "return %a;" pr_paren_exp e
  | SWITCH(e,b)    -> fprintf ppf "switch %a@,%a"
        pr_always_paren_exp e pr_subbl b
  | CASE(e,b)      -> fprintf ppf "case %a:@,%a"
        pr_exp e pr_subbl b
  | CASERANGE(e1,e2,b) -> fprintf ppf "case %a...%a:@,%a"
        pr_exp e1 pr_exp e2 pr_subbl b
  | DEFAULT b      -> fprintf ppf "default:@ %a;@," pr_subbl b
  | LABEL l        -> fprintf ppf "%s:;@," l
  | GOTO l         -> fprintf ppf "goto %s;" l
  | COMPGOTO e     -> fprintf ppf "goto *%a;" pr_paren_exp e

 (* block that is a part of if, while, etc. *)
 and pr_subbl : formatter -> block -> unit = fun ppf -> function
   | []  -> fprintf ppf "@;<0 2>;"
   | [IF _ | DOWHILE _ as s] -> fprintf ppf "{@[<v 2>%a@]}" pr_stmt s
   | [s] -> fprintf ppf "@;<0 2>%a" pr_stmt s
   | b   -> pr_block ppf b

 (*
   let pr_labels ppf = function [] -> () | ls ->
     fprintf ppf "__label__ %a@,"
        (pp_print_list ~pp_sep:pp_sep_comma pp_print_string) ls
 *)
 (* always print the braces *)
 and pr_block : formatter -> block -> unit = fun ppf -> function
   | [] -> pp_print_string ppf "{}"
   | stmts ->
       fprintf ppf "{@;<0 2>@[<v 0>%a@]@,}"
         (pp_print_list pr_stmt) stmts

let pr_decl : formatter -> declaration -> unit = fun ppf -> function
 | FUNDEF (ct,v,args,body) ->
     fprintf ppf "@,@[<v 0>%a %s@[(%a)@]%a@]@."
       pr_ctype ct v 
       (pp_print_list ~pp_sep:pp_sep_comma pr_typedname) args
       pr_block body
 | DECDEF defn -> pr_defn ppf defn
 (*
 | PROTO   of typedname * varname * typedname list  (* function prototype *)
 | TYPEDEF of typedname
 *)
 | _ -> failwith "other declarations not yet implemented"
