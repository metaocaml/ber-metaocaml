(* 
 Declaration of a simple variant type, to be used in the main
 test file vgprint.ml

 The goal is to test gprint of values whose types are declared
 in other files. To be precise, we test the proper setting of the
 load_path, needed to locate .cmi files with declarations for
 the types of the values being printed.

 This test has been proposed by Tran Minh Quang.
*)

type ext = T1 of int | T2 of string option
