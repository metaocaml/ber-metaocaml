(**			Generic print

Test code.

$Id: vgprint.ml,v 1.1 2006/04/15 11:27:37 oleg Exp $
*)

open Gprint;;

let pr_type et = Format.printf "\n%s@." et

let () = pr_type (print .<1>.)
;;


let () = pr_type (let x = 1 in print .<x>.);;

let () = pr_type (print .<10.0>.)
;;

let () = pr_type (print .<"xxx">.)
;;

let () = pr_type (let x = Some ([|(10,true);(11,false)|]) in print .<x>.);;

module C = struct
  type 'a color = Blue | Green | Rgb of 'a
end;;

type 'a image = {title : string; pixels : 'a C.color array};;
type big = int image list;;

let v = [
  {title = "im1";
   pixels = [| C.Blue; C.Rgb 10 |]};
  {title = "im2";
   pixels = [| C.Green |]};
] ;;

let () = pr_type (print .<v>.)
;;

(* current drawback due to the lack of integration, and the work-around*)
let foo (x : int option) = print .<x>. in pr_type (foo None );;

let foo (x : int option) = print (let z = [x] in .<z>.) in pr_type (foo None);;


(* Now we open C *)
open C

let some_processing ims =
  let brighten px =
      let new_px = match px with
	            Blue  -> Green
                  | Green -> Rgb 10
		  | Rgb x -> Rgb (x+1) in
      let () = Format.printf "@.pixel: %a -> %a @."
	       (fun ppf v -> ignore (fprint ppf v))
	       (let x = [px] in .<x>.)
	       (fun ppf v -> ignore (fprint ppf v))
	       (let x = [new_px] in .<x>.) in
      new_px in
  let process im =
    let () = Format.printf "Processing: " in
    let _  = print .<im>. in
    {im with pixels = Array.map brighten im.pixels} in
  let res = List.map process ims in
  let _ = print .<res>. in
  Format.printf "@."
;;

let () = some_processing v;;


(* This test gprint-ing of values whose type has been declared
   in other files. In other words, we test locating .cmi files
   at the type of gprint.

   This test has been proposed by Tran Minh Quang.
*)

let extv = Vgprint_aux.T2 (Some "extv");;

let () = pr_type (print .<extv>.)
;;

print_endline "\nAll done"
;;
