(* Abstract Syntax Tree for C

  Unlike IR used by MetaOCaml, it is designed to reflect a bigger subset
  of C, including GOTO and possibly inline assembly. Also, it is designed
  to be straightforwardly pretty-printed to C.

  It is loosely based on the `abstract syntax for FrontC' by Hugues Cassé
  (version 2.1, 4.7.99) but with many simplifications and changes

  There are two biggest simplifications. First, type specification: We do not
  follow C bizzare conventions, and do not support composite
  declarations like
   int x, *z, * const z;
  Each declaration should introduce only one variable.

  Second, we reject C's madness and do not consider assignment,
  pre/post increment/decrement and modification (e.g., +=) as expressions.
  They are statements, period. We thus do not support things like *x++ = j++ 
  etc.

  We also support C99: mixing of declarations and statements.
  Declarations don't have to be collected at the beginning of the block.

  Our data structure representation is also more precise, reflecting
  the fact that comma-expressions support a restricted set of statements.
*)

type label     = string
type varname   = string
type fieldname = string

(* C types, or subset thereof that we are using *)
(* C11 has boolean data types. From
 * https://stackoverflow.com/questions/1921539/using-boolean-values-in-c
 * ``For the datatype, #include <stdbool.h>, and use true, false and bool. 
 * Or do not include it, and use _Bool, 1 and 0 instead.''
 *)

type ctype = {typ: typ; specs: spec list}
and typ =
  | Tvoid
  | Tbool
  | Tchar
  | Tshort
  | Tint
  | Tlong
  | Tint64
  | Tuint                               (* unsigned int *)
  | Tulong                              (* unsigned long *)
  | Tfloat
  | Tdouble
  | Tnamed of string                    (* names typedef'd types, plus the
                                           catch-all for other types
                                         *)
  | Tptr of ctype
  | Tarray of int * ctype
  (* Tfun -- don't support for now *)
and spec =                              (* specifiers *)
  | S_const
  | S_volatile
  | S_restrict
  | S_static
  | S_extern
  | S_inline

(* The constants carry textual representation, to spare the
   pretty-printer having to deal with the precision of OCaml's integers, 
   etc.
 *)
type constant =
  | Const_num    of string              (* textual representation *)
  | Const_char   of char                (* may need escaping *)
  | Const_string of string              (* may need escaping *)

type binary_operator =
  | ADD | SUB | MUL | DIV | MOD
  | AND | OR
  | BAND | BOR | XOR | SHL | SHR
  | EQ | NE | LT | GT | LE | GE

type unary_operator =
  | MINUS | PLUS 
  | NOT                                 (* ! *)
  | BNOT                                (* ~ *)
  | MEMOF                               (* * *)
  | ADDROF                              (* & *)

type binary_modifier =
  | ASSIGN
  | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN | SHL_ASSIGN | SHR_ASSIGN

type unary_modifier =
  | POSINCR | POSDECR                   (* no pre-increment/pre-decrement *)

(* If an expression does not have Calls, it is pure! *)
type expression =
  | Nothing                             (* for return; etc. *)
  | Const      of constant
  | Var        of varname
  | Unary      of unary_operator * expression
  | Label_addr of label  (* GCC's && Label *)
  | Binary     of binary_operator * expression * expression
  | Cond       of expression * expression * expression
  | Cast       of ctype * expression
   (* A CAST can actually be a constructor expression *)

  | Call  of expression * expression list
  | Comma of simple_statement list * expression
  (*
  | EXPR_SIZEOF of expression
  | TYPE_SIZEOF of specifier * decl_type
  | EXPR_ALIGNOF of expression
  | TYPE_ALIGNOF of specifier * decl_type
  *)
  | Index       of expression * expression
  | Memberof    of expression * fieldname
  | Memberofptr of expression * fieldname

and init_expression =
  | Init_none
  | Init_single of expression
  | Init_many   of expression list

and simple_statement =
  | NOP
  | CALL      of expression * expression list (* procedure call *)
  | UNMOD     of unary_modifier * expression
  | BIMOD     of binary_modifier * expression * expression
and let_or_statement =
  | LET       of definition
  | STMT      of simple_statement
and statement =
  | SIMPLE    of let_or_statement
  (* The motivation for this is to restrict the scope of bindings
     in the containing expressions
  *)
  | BLOCK     of block
  | IF        of expression * block * block
  | WHILE     of expression * block
  | DOWHILE   of expression * block
  | FOR       of let_or_statement * expression * simple_statement * block
  | BREAK
  | CONTINUE
  | RETURN    of expression
  | SWITCH    of expression * block
  | CASE      of expression * block
  | CASERANGE of expression * expression * block
  | DEFAULT   of block
  | LABEL     of label                  (* label:; *)
  | GOTO      of label
  | COMPGOTO  of expression (* GCC's "goto *exp" *)
  (*
  | ASM of attribute list * (* typically only volatile and const *)
         string list * (* template *)
       (string * expression) list * (* list of constraints and expressions for 
                                   * outputs *)
       (string * expression) list * (* same for inputs *)
       string list * (* clobbered registers *)
  *)   

(* Before:
   A block contains a list of local label declarations ( GCC's ({ __label__ 
   l1, l2; ... }) ) , a list of definitions and a list of statements.
   We don't use GCC labels anyway, so we drop them.
   Also, we now support the mixture of definitions and statements
*)
and block = statement list

and typedname = varname * ctype         (*   const int * x   *)

and definition = typedname * init_expression  (* local/global variable *)

(*
 * Declarations: at the top level
*)
type declaration =
 | FUNDEF  of ctype * varname * typedname list * block
 | DECDEF  of definition                            (* global variable *)
 | PROTO   of typedname * varname * typedname list  (* function prototype *)
 | TYPEDEF of typedname
 (*
 | ONLYTYPEDEF of specifier
 | GLOBASM of string
 | PRAGMA of expression
 | INCLUDE of string
 | IFDEF of string
 | ENDIF
 *)

