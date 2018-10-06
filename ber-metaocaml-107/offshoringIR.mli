(* Another way to run code:
   Converting a simple subset of OCaml to an intermediate language 
   designed to be easily translatable to C, openCL, LLVM, etc.

   We handle only an (imperative) subset of OCaml, as typical in offshoring.
*)

(* Simple OCaml code may be considered as C written in different syntax.
   The following helps bring OCaml closer to C
*)

type float32 = private float

val float32_of_float : float -> float32

(* For-loop with a step, like the for-loop in C *)
val forloop : 
    int ->                              (* lower bound *)
    int ->                              (* upper bound +1, like in C *)
    int ->                              (* step *)
    (int -> unit) ->                    (* body *)
    unit

(* Types that we currently support
   It is a subset of the very simplified OCaml type representation
   It is much easier to deal with than OCaml types
*)

type typ = ..
type typ +=
  | TUnit
  | TInt
  | TBool
  | TFloat                              (* 32-bit *)
  | TDouble                             (* 64-bit *)
  | TArray1 of typ                      (* Usual array or Bigarray.Array1 *)
  | TArray2 of typ                      (* Bigarray.Array2 *)
  | TRef of typ
  | TVariable                           (* sometimes inevitably occurs
                                           We do not do any substitutions, etc*)
		 
type varname = private string 
type constant_t = Asttypes.constant

type attribute = ..

type exp =
  | Const    of constant_t              (* Constant/literal: int, bool,...*)
  | LocalVar of varname * typ           (* Locally-bound variable *)
  | KnownVar of varname                 (* Global/library function,... *)
  | FunCall  of varname * exp list      (* Calls only to known identifiers *)
  | Let      of {id: varname; ty: typ; bind: exp; body: exp;
                 attrs: attribute list}
  | IfE      of exp * exp * exp
 and cmd =
  | FunCallC of varname * exp list      (* Calls only to known identifiers *)
  | LetC      of {id: varname; ty: typ; bind: exp; body: cmd;
                 attrs: attribute list}
  | If       of exp * cmd * cmd option
  | For      of {id: varname; ty:typ;
                 lwb: exp; upb: exp; step: exp; body: cmd}
  | While    of exp * cmd 
  | Seq      of cmd * cmd

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
  val id_conv : pathname -> varname -> string
end

(* Default conversion module. The programmer is supposed to
   include it and override what is needed
*)
module DefaultConv : converters


(* Main offshoring function: convert from the code in the supported 
   subset of OCaml to the intermediate language.
   Return either cmd (for procedure with no result) or exp and the
   result type.
*)
type args_t = (varname * typ) list
type proc_t = 
  | Fun  of args_t * typ * exp           (* Function with the result *)
  | Proc of args_t * cmd                 (* Procedure, no result     *)
val offshore : (module converters) -> 'a Codelib.code -> proc_t



