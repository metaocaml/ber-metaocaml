(* Quick test of the native metaocaml: The example of run-time specialization *)
open Square

(* The standard power function: x^n *)

let rec power : int -> float -> float = fun n x ->
  if n = 0 then 1.
  else if n mod 2 = 0 then square (power (n/2) x)
  else x *. (power (n-1) x)

(* Staged power *)
let rec spower : int -> float code -> float code = fun n x ->
  if n = 0 then .<1.>.
  else if n mod 2 = 0 then .<square .~(spower (n/2) x)>.
  else .<.~x *. .~(spower (n-1) x)>.

let spowern n = .<fun x -> .~(spower n .<x>.)>.

(* Benchmark *)
let perf : (unit -> 'a) -> 'a = fun th ->
  let start_time = Sys.time () in
  let r = th () in
  let elapsed_time = Sys.time () -. start_time in
  Printf.printf "\nit took %g secs\n" elapsed_time;
  r

let _ =
  let go count n x = 
    Printf.printf "Unspecialized power %g ^ %d is %g\n"
      x n (perf (fun () -> for i = 1 to count do ignore (power n x) done;
                           power n x));
    let spower_fn = Runnative.run (spowern n) in
    Printf.printf "Specialized power %g ^ %d is %g\n"
      x n (perf (fun () -> for i = 1 to count do ignore (spower_fn x) done;
                           spower_fn x))
  in
  go 100000 100000 1.00001

  (* Printf.printf "Enter iteration count, n and x:%!";
     Scanf.scanf "%d %d %g" go

  let _ = main ()
   *)

(* Good values of the arguments: 10^6 for count, 10^5 for n, 1 + 10^(-5) for x *)
