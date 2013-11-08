(*
   To `run' the code we use toplevel facilities.
   If we invoke BER MetaOcaml top level, then Toplevel.topstart() will
   initalialize the top level.
   If we execute a byte-compiled executable, we link with
   the top-level library. But we need initialize it first.
   This is the job of the current file.

   This file must be linked in *before* the first user executable.

   The present code roughly do the same steps OCaml top level does
   when executing a script.
   See Toplevel.topmain
*)

let () =
  Toploop.set_paths ();
  Compile.init_path();
  Toploop.initialize_toplevel_env ()
  (* toplevel_env := Compile.initial_env();
  *)
