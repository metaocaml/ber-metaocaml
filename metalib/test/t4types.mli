(* Data type declarations for the staged eval example by 
   Walid Taha and Cristiano Calcagno

In MetaOCaml N100, all data types used within brackets must be 
in separate .ml or .mli files
*)

type exp = I of int
         | V of string
         | A of exp * exp
         | L of string * exp
         | D of exp
         | C of exp * exp * exp
         | R of string * string * exp

type dom = J of int
         | F of (dom -> dom)
