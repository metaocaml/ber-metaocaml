(* Code Prelude
   The initially open module in MetaOCaml

   It describes the code handling facilities other than brackets and escapes.
   (Running the code is in separate runcode and runnative modules)

   This present module applies to both native and bytecode modes 
*)

open Format

type 'a closed_code = Trx.closed_code_repr

type 'a code     = 'a Trx.code
type 'a pat_code = 'a Trx.pat_code
type 'a val_code = 'a Trx.val_code

(* What to do about hardly serializable CSPs *)
type csp_handling = Trx.csp_handling = CSP_ok | CSP_warn | CSP_error

(* Check that the code is closed and return the closed code *)
let close_code : ?csp:csp_handling -> 'a code -> 'a closed_code = 
  fun ?csp cde ->
  Trx.close_code_repr ?csp (cde :'a code :> Trx.code_repr)

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code.
*)
let close_code_delay_check : 
  ?csp:csp_handling -> 'a code -> 'a closed_code * (unit -> unit) =
  fun ?csp cde -> 
    Trx.close_code_delay_check ?csp (cde : 'a code :> Trx.code_repr)

let open_code : 'a closed_code -> 'a code = fun ccde ->
  Obj.magic (Trx.open_code ccde)

(* The original code was authored by  Ed Pizzi
   and simplified by Jacques Carette.
   It is latter borrowed into the main OCaml tree,
   as parsing/pprintast.ml.
   It was extensively rewritten by Hongbo Zhang: University of Pennsylvania
   and modified by Thomas Gazagnaire (OCamlPro) and
   Fabrice Le Fessant (INRIA Saclay).

   We now rely on the OCaml's code.
*)


(* print code as a parse tree. Useful for debugging *)
let print_code_as_ast cde =
  let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
  Printast.implementation Format.std_formatter
  [Ast_helper.Str.eval cde]

let format_code : Format.formatter -> 'a closed_code -> unit = fun ppf cde ->
  let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
  Pprintast.expression ppf cde

(* These functions are suitable for installing as printers
   at the toplevel, using top-level directive install printer.
   Don't rename these functions or change their types.
   See bertop.ml, which refers to these functions by their external
   symbolic name.
*)

let print_closed_code  : Format.formatter -> 'a closed_code -> unit = 
  fun ppf cde ->  
    Format.fprintf ppf ".<@,%a>.@ " format_code cde

let print_code ppf (cde : 'a code) = 
  let (cde, check) = close_code_delay_check ~csp:CSP_warn cde in
  print_closed_code ppf cde;
  try check ()
  with e -> fprintf ppf "\n%s" (Printexc.to_string e)

(* `Format' close code value as an AST *)
let ast_of_code : 'a closed_code -> Parsetree.expression = fun cde ->
 (cde : Trx.closed_code_repr :> Parsetree.expression)

(* Utility functions for code construction *)
(* sequencing *)
let seq : unit code -> 'a code -> 'a code = fun x y -> .<.~x; .~y>.

(* Utility: build a code for a sequence:
   folding over unit code monoid
 *)
let rec seqs : unit code list -> unit code = function
  | []   -> .<()>.
  | [x]  -> x
  | h::t -> seq h (seqs t)

(* First-class pattern-match. See Trx.make_match for details.
*)
let make_match : 'a code -> ('a -> 'w) pat_code list -> 'w code =
  fun[@warning "-8"] scrutinee cases ->
    let scrutinee = (scrutinee : 'a code :> Trx.code_repr) in
    let cases = 
      List.map (fun x -> (x : ('a -> 'w) pat_code :> Trx.code_repr))
       cases in
    Obj.magic (Trx.make_match scrutinee cases)

(* A different approach to first-class pattern-matching. Although it seems
   to be working out, the implementation is a chore. The existing approach
   seems to do what I wanted anyway. So, the following is just for the record.

module type make_match = sig
  type ('e,'a) pat
  type ('a,'w) clause

  type 'e varseq

  val vars1 : 'a code varseq
  val vars2 : ('a code * 'b code) varseq
  val vars3 : ('a code * ('b code * 'c code)) varseq
  val varss : 's varseq -> ('a code * 's) varseq

  val const : 'a code -> (unit,'a) pat
  val var   : unit -> ('a,'a) pat
  val pat   : 'e varseq -> ('e -> 'a code) -> ('e,'a) pat
  val alias : ('e,'a) pat -> (('a code * 'e),'a) pat

      (* later add exception : ('e,'a) pat -> ('e,'a) pat *)
  val clause : ?guard:('e -> bool code) -> ('e,'a) pat ->
                ('e -> 'b code) -> ('a,'b) clause

  val make_match    : 'a code -> ('a,'w) clause list -> 'w code 
  val make_function : ('a,'w) clause list -> ('a -> 'w) code 
end

module Match : make_match = struct
  type ('e,'a) pat = Parsetree.pattern * string list * (Trx.code_repr list -> 'e)

  type ('a,'w) clause = 
       {vars : string list;             (* variables in the pattern *)
        pat  : Parsetree.pattern;
        guard : (Trx.code_repr list -> Trx.code_repr) option;
        rhs   : (Trx.code_repr list -> Trx.code_repr)
      }

  type 'a varseq = (unit -> string list) * (Trx.code_repr list -> 'a)

  let new_varcode : Trx.code_repr -> 'a code = Obj.magic
  let gensym : string -> string = fun _ -> failwith "na"
  let to_pat : 'a code -> Parsetree.pattern = fun cde -> failwith "na"
  let new_var : string -> Trx.code_repr = failwith "na"
  let[@warning "-8"] vars1 : 'a code varseq = 
    (fun () -> [gensym "x"]), (fun [x] -> new_varcode x)
  let[@warning "-8"] vars2 : ('a code * 'b code) varseq = 
     ((fun () -> [gensym "x";gensym "y"]),
     (fun [x;y] -> (new_varcode x,new_varcode y)))
  let[@warning "-8"] varss : 's varseq -> ('a code * 's) varseq = 
    fun (gs,subst) ->
    ((fun () -> gensym "u" :: gs ()),
     (fun (h::t) -> (new_varcode h, subst t)))
  let vars3 : ('a code * ('b code * 'c code)) varseq = varss vars2

  let[@warning "-8"] const : 'a code -> (unit,'a) pat = fun cde ->
    (to_pat cde, [], (fun [] -> ()))

  let[@warning "-8"] var   : unit -> ('a,'a) pat = fun () ->
    failwith "na"

  let pat   : 'e varseq -> ('e -> 'a code) -> ('e,'a) pat = 
    fun (newnames,cnv) patf ->
      let names = newnames () in
      (patf (cnv (List.map new_var names)) |> to_pat, names, cnv)

  let alias : ('e,'a) pat -> (('a code * 'e),'a) pat = fun pat -> 
    failwith "na"

  let clause : ?guard:('e -> bool code) -> ('e,'a) pat ->
                ('e -> 'b code) -> ('a,'b) clause =
    fun ?guard (pat,vars,cnv) rhs ->
      {pat; vars; 
       rhs = (fun vars -> (rhs (cnv vars) : _ code :> Trx.code_repr));
       guard = guard |>
       Option.map (fun g vars -> (g (cnv vars) : bool code :> Trx.code_repr))}

  let split : int -> 'a list -> 'a list * 'a list = fun n lst ->
      let rec loop n acc lst = match (n,lst) with
      | (0,lst)  -> (List.rev acc,lst)
      | (n,h::t) -> loop (n-1) (h::acc) t
      | _        -> assert false
      in loop n [] lst

  let make_match    : 'a code -> ('a,'w) clause list -> 'w code =
    fun cde cl ->
      let pon = 
        List.(map (fun {pat} -> pat) cl,
              rev @@ fold_left (fun acc {vars} ->
                acc |> rev_append (map Location.mknoloc vars)) [] cl) in
      let fgbodies var_arr =
        let (allvars,cl') =
        List.fold_left (fun (allvars,acc) {vars;guard;rhs} ->
          let (newvars,allvars) = split (List.length vars) allvars in
          (allvars, (Option.map ((|>) newvars) guard, rhs newvars) :: acc))
          (Array.to_list var_arr, []) cl
        in assert (allvars = []); List.rev cl' |> Array.of_list
      in
      Obj.magic @@
      Trx.build_match Location.none pon (cde : _ code :> Trx.code_repr) 
                      (List.length cl) fgbodies 

  let make_function : ('a,'w) clause list -> ('a -> 'w) code = fun cl ->
    failwith "na"
end

module M(S:make_match) = struct
  open S
  (* The `ideal' function that doesn't work as we wanted it to *)
  let remove : int -> int list -> int list = fun x l ->
    let rec loop = function
      | [] -> []
      | x::t -> loop t
      | h::t -> h :: loop t
    in loop l

  let sremove : int -> (int list -> int list) code = fun x ->
    .<let rec loop l = .~(make_match .<l>. [
        clause (const .<[]>.) (fun _ -> .<[]>.);    (* then try as x *)
        clause (pat vars1 (fun t -> .<x :: .~t>.)) (fun t -> .<loop .~t>.);
        clause (pat vars2 (fun (h,t) -> .<.~h :: .~t>.)) 
               (fun (h,t) -> .<.~h :: loop .~t>.);
        ])
    in loop>.

  let sremove' : int -> (int list -> int list) code = fun x ->
    .<let rec loop l = .~(make_match .<l>. [
        clause (alias (const .<[]>.)) (fun (x,_) -> x);
        clause (pat vars1 (fun t -> .<x :: .~t>.)) (fun t -> .<loop .~t>.);
        clause (pat vars2 (fun (h,t) -> .<.~h :: .~t>.)) 
               (fun (h,t) -> .<.~h :: loop .~t>.);
        ])
    in loop>.
end
*)

(* let-insertion *)
type locus = Trx.locus
let locus_global = Trx.locus_global

let genletv : ?name:string -> ?locus:locus -> 'a code -> 'a val_code = 
  fun ?(name="t") ?(locus=locus_global) cde -> 
  let cde = (cde : 'a code :> Trx.code_repr) in
  Obj.magic @@ Trx.genlet name locus cde

(* The result of genletv coerced to 'a code so that it can
   be immediately used in a splice. This is a very common pattern.
*)
let genlet : ?name:string -> ?locus:locus -> 'a code -> 'a code = 
  fun ?name ?locus cde -> 
  (genletv ?name ?locus cde : 'a val_code :> 'a code)

let with_locus : (locus -> 'w code) -> 'w code = fun k ->
  Trx.with_locus (fun l -> (k l : _ code :> Trx.code_repr)) |> Obj.magic

(*Immediate let-insertion: generates the let-expression right-away
  However, if the expression to bind is simple (is a value whose construction
  does not cause big memory allocations), no let is generated.
  It is essentially 
  with_locus (fun locus -> genlet locus exp |> k)
  One may say that the generated let is immediately beta-reduced away.
  The optional ?name is the hint about the generated name
 *)
let letlv : ?name:string -> 'a code -> (('a val_code -> 'w code) -> 'w code) =
  fun ?(name="t") cde k ->
  let cde = (cde : 'a code :> Trx.code_repr) in
  Obj.magic @@ 
  Trx.genlet_immediate name cde 
    (fun cv -> (k (Obj.magic cv) : 'w code :> Trx.code_repr))

(* The result of letlv coerced to 'a code so that it can
   be immediately used in a splice. This is a very common pattern.
*)
let letl : ?name:string -> 'a code -> (('a code -> 'w code) -> 'w code) =
  fun ?name cde k ->
    letlv ?name cde (fun cv -> k (cv : 'a val_code :> 'a code))

(* let rec insertion:
   Potentially mutually recursive memoizing let-insertion

   See PEPM 2019 paper for more details and discussion.
   The interface and implementation below are 
   inspired by the formal development (ML 2019)
*)

type _ memo_key = ..

type 'a binding = 
      'a val_code *        (* bound var *)
      'a code option ref   (* binding, possibly not yet determined *)

type mbinding = 
    B : 'a memo_key * 'a binding      (* Memoized binding, 'a is exist-ed *)
      -> mbinding

type locus_rec = 
    {genvar: string -> Trx.code_repr;   (* free variable generator *)
     bindings: mbinding list ref}

(* The maker of genletrec *)
let mkgenlet : type key a b. ?name:string -> locus_rec -> 
    (key->key->bool) ->               (* memo key comparison *)
    (* The genletrec function itself, which takes a key and 
       a function to produce the expression to let-bind
    *)
    ((key -> (a->b) code) -> key -> (a->b) code) = 
  fun ?(name="h") l eqp ->
  let module M = struct
    type _ memo_key += Tag : key -> (a->b) memo_key  
    let inj  : key -> (a->b) memo_key = fun x -> Tag x
    let find : key -> mbinding list -> (a->b) binding option = fun k lst ->
      let rec loop : mbinding list -> (a->b) binding option = function
        | [] -> None
        | B (Tag k',x)::_ when eqp k k' -> Some x
        | _::t -> loop t
      in loop lst
    end
  in
  let ret var = (var : (a->b) val_code :> (a->b) code) in
  (* the genletrec function itself *)
  fun exp key ->  
  match M.find key !(l.bindings) with 
  | Some (var,_) -> ret var
  | None -> 
      let var = (Obj.magic (l.genvar name) : (a->b) val_code) in
      let bexpr = ref None in
      let () = l.bindings := B (M.inj key,(var,bexpr)) :: !(l.bindings) in
      let bexp = exp key in
      match !bexpr with
      | Some _ -> ret var               (* already bound *)
      | None   -> (bexpr := Some bexp; ret var)

(* Convert accumulated mbinding to the proper bindings
 *)
let with_locus_rec : (locus_rec -> 'w code) -> 'w code = fun body ->
  let k genvar =
    let locus = {genvar; bindings = ref []} in
    let exp = body locus in
    let bindings =
      !(locus.bindings) |>
      List.map (function B (_,(vi,{contents = Some expi})) -> 
        ((vi   : _ val_code :> Trx.code_repr),
         (expi : _ code :> Trx.code_repr))
        | _ -> failwith "genletrec_locus: unbound var!")
    in ((exp : _ code :> Trx.code_repr), bindings)
  in
  Obj.magic @@ Trx.make_genletrec k
 
(* Liftable types
   The types of present-stage values that are OK to use at a future
   stage (values that are safe to CSP, both in byte- and native-code modes)
*)

module type lift = sig
  type t
  val lift: t -> t code
end
