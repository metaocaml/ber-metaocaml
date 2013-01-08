(* time n s f   executes f() n times and records message s plus elapsed time *)
val time : int -> string -> (unit -> 'a) -> 'a
val timenew : string -> (unit -> 'a) -> 'a
val init_times : unit -> unit
(* flushes recorded times *)
val print_times : unit -> unit
val get_times : unit -> ((string * int * float) list)

(* To be activated later....
  
(* More precise timing functions *)
type ticks


external times: unit -> float * float = "sys_times"
(* returns user and system process times, in whole and fractional seconds.
   Resolution upto a microsecond.
 *)

external get_ticks: unit -> ticks = "sys_get_ticks"
(* Gets the CPU cycle counter as an abstract object. See cycle.h
   from the FFTW project *)

external elapsed_ticks: ticks -> float = "sys_elapsed_ticks"
(* Returns the elapsed time in abstract units from the
   specified moment till now. *)

val timecycles: string -> (unit -> 'a) -> 'a
(* like timenew but uses the CPU cycle counter for timing. 
   It's better for shorter functions that can run without being
   scheduled out.
*)

*)
