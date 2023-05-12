(* Mock GPU module, for the offshoring test.
   It defines the functions that are used in GPU kernels
   (with some appropriate behavior in OCaml)
*)

let get_group_id   : int -> int = fun x -> 0
let get_num_groups : int -> int = fun x -> 128
let get_local_id   : int -> int = fun x -> 0
let get_local_size : int -> int = fun x -> 64
