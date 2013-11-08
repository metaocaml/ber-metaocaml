let print_buffer = ref []
let numbers_buffer = ref []

let init_times  () = 
  let _ = print_buffer := [] in
  numbers_buffer := []

(* helper function to print times consisently *)

let pt s reps elapsed =
  let t = String.sub (s^" ___________________") 0 20 in
  let reps_string = "_________ "^(string_of_int reps) in
  let len         = String.length reps_string in
  let reps_trimed = String.sub reps_string (len-10) 10 in
  let _ = numbers_buffer := (s, reps, elapsed) :: !numbers_buffer in
  print_buffer := (fun () -> (Format.fprintf Format.std_formatter
                               "__ %s_%sx avg= %E msec"
                                 t reps_trimed elapsed; (Format.print_newline ()))) :: !print_buffer

(* do total time *)
let time reps s f =
  let initial = Sys.time() in
  let _       = while initial = (Sys.time()) do () done in
  let initial = Sys.time() in
  let result = f () in
  let final =
     (for i=1 to reps-1 do f() done;
      Sys.time ()) in
  let elapsed = (final -. initial) *. 1000.0 /. (float reps) in
  pt s reps elapsed;
  result

let timenew s f =
  let mx  = ref 0 in  (* Variables for fine timing construction *)
  let n  = ref 0 in
  let m  = ref 0 in
  let s1 = ref 0.0 in
  let s2 = ref 0.0 in
  let s3 = ref 0.0 in
  let _ = m:=0 in
  let _ = mx:=0 in
  let _ = s2:= (0.0) in
  let _ = s3:= (0.0) in

  let reps = ref 1 in (* adaptive timing code *)
  let tr   = ref 0.0 in
  let t1 = Sys.time() in
  let t2 = begin while (Sys.time() = t1) do () done; Sys.time() end in
  let r  = f () in
  let _  = (tr:=Sys.time()) in
  let _  = while (!tr -. t2 < 1.0)
           do for i=1 to !reps
              do f () done;
           reps := (!reps)*2;
           tr   := Sys.time ()
           done in (* end adaptive timing code *)

let _ = s1:= !tr in  (* back to fine timing code *)
let _ = for j=1 to 1 do
        while (Sys.time () = (!s1)) do
           for i=1 to 100 do
            n := (!n) + 1
          done
         done; s2:=(Sys.time ())
        done in
let _ = s1:=!s2 in
let _ = for j=1 to 10 do
        while (Sys.time () = (!s2)) do
          for i=1 to 100 do
            m := (!m) + 1
          done
         done; s2:=(Sys.time ());
         mx:= max !m !mx;
         m:=0;
         done; in
let p  = (float !n)/.(float !mx) in
let te = (!tr*.(1.0-.p)) +. (!s1*.p) in

  let dt = (te -. t2) in
  let n  = !reps in
  let at = dt *. 1000.0 /. (float n) in
  pt s n at;
  r

let print_times () =
  List.iter (fun f -> f ()) (List.rev !print_buffer);
  print_buffer := []

let get_times () =
	let nums = (List.rev !numbers_buffer) in
	let _ = numbers_buffer := [] in
		nums

(* To be activated later

(* More precise timing functions *)
type ticks

external times: unit -> float * float = "sys_times"
external get_ticks: unit -> ticks = "sys_get_ticks"
external elapsed_ticks: ticks -> float = "sys_elapsed_ticks"

(** Timing based on CPU cycles.
   CPU cycle counter is a real timer; it counts in CPU cycles, whose
   period is the inverse of the rated frequency of the CPU.
   The CPU cycle counter is most useful for shorter functions: for longer
   functions, CPU cycle counter sees the effect of time interrupts and
   preemption. Unlike the timing returned by Sys.time or
   times, the cycle counter is real. That is, it counts even
   when other process has the control of the CPU. CPU cycle counter
   sees the effect of caching, etc.
 
  See
   http://cedar.intel.com/software/idap/media/pdf/rdtscpm1.pdf
  for more details on cycle counter.

  For consistency with the other timing interface, our function should
  invoke 'pt s n at' and pass the number of invocations of the
  function to time (n) and the elapsed time, in _milli_seconds.
  Therefore, we have to deterimine the calibrartion coefficient from
  cycles to seconds.

  To avoid the interference from time interrupts and scheduler
  pre-emptions, we repeat the timing measurements to make the elapsed
  time last up to 9 milliseconds, assuming a 100 Hz scheduling clock.
  We accumulate the count of all the executions of the functions and
  return the second smallest. We do a few dry runs of the function to
  warm up the caches.
*)

(* We target our cycle counters to last no more than the following number
   of seconds.
   The number is 90% of the scheduling interval (typically 10 ms).
   It's quite likely that the current process will be preempted after
   it consumes its quantum, and its priority lowered.
*)
let cycle_timing_target = 0.009


(* Cycles to seconds calibration code.
   get_cycles_per_second: () -> float
   Return the number of cycles per second.
*)

let get_cycles_per_second () =
  let calib_fn n = for i=1 to n do let _ = sin 0.8 in () done in
  let repeat = 5 in		(* How many times to repeat everything*)
  let n_target =		(* How many times to repeat calib_fn*)
      let () = calib_fn 10 in		(* warm up the cache *)
      let rec loop n1 n2 = 
          let (tbeg,_) = times () in
          let () = calib_fn n2 in
          let exp = fst (times ()) -. tbeg in
          if exp > cycle_timing_target then
             if n1 = n2 then n1 else 
             if exp > 1.09 *. cycle_timing_target 
             then loop n1 ((n1 + n2)/2) else n2
          else loop n2 (2*n2) in
      loop 1000 1000
  in
  let do_cycles () =
   let () = calib_fn 10 in		(* warm up the cache *)
   let cbeg = get_ticks () in
   let () = calib_fn n_target in
   elapsed_ticks cbeg
  in
  let do_times () =
   let () = calib_fn 10 in		(* warm up the cache *)
   let (tbeg,_) = times () in
   let () = calib_fn n_target in
   fst (times ()) -. tbeg
  in
  let cycles_est =			(* the second smallest datum *)
   let d1 = do_cycles () in
   let d2 = do_cycles () in
   let rec loop d1 d2 n =
      if n = 0 then d2 else
      let d = do_cycles () in
      if d < d1 then loop d d1 (n-1) else
      if d < d2 then loop d1 d (n-1) else loop d1 d2 (n-1) in
   loop (min d1 d2) (max d1 d2) repeat
  in
  let secs_est =			(* the second smallest datum *)
   let d1 = do_times () in
   let d2 = do_times () in
   let rec loop d1 d2 n =
      if n = 0 then d2 else
      let d = do_times () in
      if d < d1 then loop d d1 (n-1) else
      if d < d2 then loop d1 d (n-1) else loop d1 d2 (n-1) in
   loop (min d1 d2) (max d1 d2) repeat
  in
   (* taking the second smallets datum for secs_est seems to give
      more robust calibration coefficient, with smaller variance. Therefore,
      the following averaging code is commented out. *)
(*   let secs_est1 = *)
(*    let rec loop accum n = *)
(*        if n = 0 then accum else *)
(*        loop (accum +. do_times ()) (n-1) *)
(*    in (loop 0.0 repeat) /. (float repeat) *)
  cycles_est /. secs_est
      
let cycles_per_second = ref 0.0

(* execute function f reasonable number of times and obtain the timing
   data, based on the cycles counter.
   s is an identification string.
   return the result of f.
   We use the function "pt s n at" to record the timing info.
   The last argument of pt is timing in _milliseconds_!
   We try to run f as many times to be close to 
   cycle_timing_target as possible. It makes no sense
   to run f() for longer intervals as we inevitably get scheduled out.
*)

let timecycles s f =
   let () = if !cycles_per_second = 0.0 then
               cycles_per_second := get_cycles_per_second () else () in
   let rough_timing =			(* in secs *)
       let _ = f () in
       let _ = f () in
       let cbeg = get_ticks () in
       let _ = f () in
       (elapsed_ticks cbeg) /. !cycles_per_second in
   let short_function = rough_timing < cycle_timing_target /. 50.0 in
   let count_major = if short_function then
                     cycle_timing_target /. (5.0 *. rough_timing) else
                     cycle_timing_target /. rough_timing in
   let count_major = max (int_of_float count_major) 5 in
   let do_cycle f =			(* the second smallest datum *)
       if short_function then
          let _ = f () in 
          let _ = f () in 		(* warm up the caches *)
          let d1 = let cbeg = get_ticks () in 
                       f(); f(); f(); f(); f(); elapsed_ticks cbeg in
          let d2 = let cbeg = get_ticks () in 
                       f(); f(); f(); f(); f(); elapsed_ticks cbeg in
          let rec loop d1 d2 n =
            if n = 0 then d2 else
            let d = let cbeg = get_ticks () in 
                       f(); f(); f(); f(); f(); elapsed_ticks cbeg in
            if d < d1 then loop d d1 (n-1) else
            if d < d2 then loop d1 d (n-1) else loop d1 d2 (n-1) in
            loop (min d1 d2) (max d1 d2) (count_major-2)
       else 
          let _ = f () in 		(* warm up the caches *)
          let d1 = let cbeg = get_ticks () in f(); elapsed_ticks cbeg in
          let d2 = let cbeg = get_ticks () in f(); elapsed_ticks cbeg in
          let rec loop d1 d2 n =
            if n = 0 then d2 else
            let d = let cbeg = get_ticks () in f(); elapsed_ticks cbeg in
            if d < d1 then loop d d1 (n-1) else
            if d < d2 then loop d1 d (n-1) else loop d1 d2 (n-1) in
            loop (min d1 d2) (max d1 d2) (count_major-2)
   in
   let result = f () in
   let count_raw = do_cycle f in
(*
   let count_dummy = do_cycle (fun () -> ()) in
   let count = if short_function then (count_raw -. count_dummy) /. 5.0
               else (count_raw -. count_dummy) in
*)
   let count = if short_function then count_raw /. 5.0
               else count_raw in
   let () = pt s (if short_function then 5 else 1)
                 (1000.0 *. count /. !cycles_per_second) in
   result
;;

*)
