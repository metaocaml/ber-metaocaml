(* Test lifting *)

open Lifts

let test_lift (type a) : (module lift with type t = a) -> a -> unit =
  fun (module L) x ->
    let cde = .<if true then .~(L.lift x) else .~(L.lift x)>. in
    Format.fprintf Format.std_formatter 
      "Generated code is as follows: @,%a@." print_code cde;
    assert (x = Runcode.run cde)

let _ = Printf.printf "Lifting\n";;

let () = test_lift (module Lift_int) 42
let () = test_lift (module Lift_char) 'y'
let () = test_lift (module Lift_float) 4.2
let () = test_lift (module Lift_string) "abc"

let () = test_lift (module Lift_option(Lift_int)) (Some 4)
let () = test_lift (module Lift_option(Lift_int)) None

let () = test_lift (module Lift_list(Lift_option(Lift_string))) 
    [None; Some "abc"; Some "xyz"]

let () = test_lift (module Lift_array(Lift_float)) 
    [| 1.0; 2.0; 3.0 |]

let () = test_lift (module Lift_array(Lift_list(Lift_string))) 
    [| []; ["1"]; ["2";"3"] |]

let () =
  let cde = .<fun x -> 
    Array.fold_left (+) 0 .~(lift_array [|.<x>.;.<1>.;.<x>.;.<2>.|])>. in
    Format.fprintf Format.std_formatter 
      "Generated code is as follows: @,%a@." print_code cde;
    assert (5+1+5+2 = Runcode.run cde 5)
  

let _ = Printf.printf "\nAll Done\n";;

