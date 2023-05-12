(* Offshoring IR

   (AST of the) simple imperative statement-oriented language: 
   the target of MetaOCaml offshoring or tagless-final combinators
   The language is designed to be pretty-printable to C (or Fortran
   or other such language) -- in full, and easily

   For more details, see `Generating C'
*)

type float32 = private float

val float32_of_float : float -> float32

(* For-loop with a step, like the for-loop in C *)
val forloop : 
    int ->                              (* lower bound *)
    upe:int ->                          (* upper bound, exclusive, like in C *)
    step:int ->                         
    (int -> unit) ->                    (* body *)
    unit


type numtyp = I32 | I64 | F32 | F64     (* Numeric types *)

type typ = ..
type typ +=
  | TVoid                               (* No values of that type *)
  | TNum of numtyp
  | TBool
  | TChar
  | TArray1 of typ                      (* Usual array or Bigarray.Array1 *)
  | TArray2 of typ                      (* Bigarray.Array2 *)
  | TRef of typ
  | TString
		 
type varname = private string 
(* Operations. Some are specified explicitly, for portability
   They follow the operations in C and C standard libraries.
   Many other languages (say, OCaml) support the same operations
   Many are indexed by type: cf WASM, where there is i32.add, i64.add,
   f32.add, f64.add
 *)
module OP : sig
   type t =
   | ADD of numtyp  | SUB of numtyp 
   | MUL of numtyp  | DIV of numtyp | MOD of numtyp
   | BAND of numtyp | BOR of numtyp | XOR of numtyp  (* bitwise operations *)
   | SHL of numtyp  | SHR of numtyp
   | EQ of numtyp   | NE of numtyp 
   | LT of numtyp   | GT of numtyp | LE of numtyp | GE of numtyp
   | AND | OR     (* these are used mostly internally. In IR, we create
                     a special node for shortcut applications *)
   | NEG of numtyp  | NOT | BNOT of numtyp
   | ASSIGN of typ
   | INCR of numtyp | DECR of numtyp        (* increment/decrement *)
   | CAST of {from: numtyp; onto: numtyp}
   | Assert
   | DEREF of typ      | REF of typ         (* typ = type of content  *)
   | Array1_get of typ | Array1_set of typ  (* typ = array element type *)
   | Other of varname
   val name : string -> t
end


(* Numeric constants should be serialized, not to lose precision,etc
   The serialization format is C or OCaml numerals (the common part)
 *)
type constant_t =                       (* no constants of void type! *)
  | Const_num    of numtyp * string     (* appropriately serialized *)
  | Const_bool   of bool
  | Const_char   of char
  | Const_string of string

type attribute = ..

(* A sequence, with easy concatenation *)

module Sq : sig
  type 'a t
  val empty : 'a t 
  val one   : 'a -> 'a t
  val (@)   : 'a t -> 'a t -> 'a t
  val iter  : ('a -> unit) -> 'a t -> unit
  val all   : ('a -> bool) -> 'a t -> bool
  val fold_right : ('a -> 'z -> 'z) -> 'z -> 'a t -> 'z
  val concat     : 'a t list -> 'a t
 end

type mutble = Mut | Cnst

(* As the data type declaration make it clear, all let-bindings 
   are `straightened-out':
   There are no nested let-bindings.
   Sequences are also straightened-out
 *)
(* exp is a simple expression, _without_ any local bindings *)
type exp =
  | Const     of constant_t              (* Constant/literal: int, bool,...*)
  | Array     of exp list                (* immediate array *)
  | LocalVar  of varname                 (* Locally-bound variable *)
  | MutVar    of varname                 (* Reference to a mutable var *)
  | GlobalVar of varname                 (* Global var,... *)
  | FunCall   of OP.t * exp list         (* Calls only to known identifiers *)
  | Cond      of exp * exps * exps       (* Conditional expression *)
  | And       of exp * exps              (* && *)
  | Or        of exp * exps              (* || *)
 (* Many contexts in C permit a comma-separated 
    non-empty sequence of expressions:
    still with no local bindings.
    Such sequences are normally imperative, and we use them in 
    if branches or while-tests, which may be executed 0 or more than 1 time.
    When we offshore e1 && e2, the local bindings in e1 may be lifted out;
    but not in e2. That's why the result of offshoring is denoted by
    And of exp * exps. Likewise, in while e do ... done, the local bindings
    from e cannot be lifted out. 
 *)
 and exps = exp list
      (* A basic element of a flow-chart of sorts *)
 and stmt   =
  | Exp      of exp
  | If       of exp * block * block option
  | While    of exps * block 
  (* Like in OCaml (but unlike C), init, upe, step are evaluated only once,
     in unspecified order
     upe is the exclusive upper-bound, like in C
   *)
  | For      of {id: varname; ty:typ;
                 guard: exp option;
                 lwb: exp; upe: exp; step: exp; body: block}
  (* Invariant: In Seq (b1,b2), b1 is not an Exp statement with empty bindings
     Also, b1 and b2 are not Unit
  *)
  | Seq      of block * block
 and block  = 
  | Unit 
  | Block of binding Sq.t * stmt
      (* (Some v, exp) corresponds to let v = exp in ...
         (None, exp)   corresponds to let () = exp in ...
         By the grammar, exp has no internal bindings.
         The expression to bind is always simple: not a sequence
       *)
 and binding = bv_desc option * exp
 and bv_desc = {id: varname; ty: typ; mut: mutble; attrs: attribute list}


(* Complete procedure
   The typ may be TVoid (in which case the it is the procedure).
   Otherwise, the block is not Unit and its last element is
   Exp
*)
type args_t = (varname * typ) list
type proc_t = args_t * typ * block

(* Utilities *)

(* New local varname with a given stem *)
val genvarname : string -> varname

val local_name  : string -> varname

val of_exp : ?bindings:binding Sq.t -> exp -> block
val dummy_binding : binding -> bool

(* Sequencing: enfocing the invariant of Seq (b1,b2) *)
val seq : block -> block -> block
