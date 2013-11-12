(* Printing code expressions *)
(* Most of the code is authored by:  Ed Pizzi *)
(* and simplified by Jacques Carette *)

open Format
open Runcode
open Parsetree

(*
open Format
open Parsetree
(* open Pprintast *)

open Asttypes
open Location
open Lexing

(* borrowed from printast.ml *)
let fmt_position f l =
  if l.pos_fname = "" && l.pos_lnum = 1
  then fprintf f "%d" l.pos_cnum
  else if l.pos_lnum = -1
       then fprintf f "%s[%d]" l.pos_fname l.pos_cnum
       else fprintf f "%s[%d,%d+%d]" l.pos_fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)

let fmt_location f loc =
  fprintf f "(%a..%a)" fmt_position loc.loc_start fmt_position loc.loc_end;
  if loc.loc_ghost then fprintf f " ghost"
(* end borrowing *)

let indent    = 1     (* standard indentation increment *)
let bar_on_first_case = true

(* These sets of symbols are taken from the manual. However, it's
   unclear what the sets infix_symbols and prefix_symbols are for, as
   operator_chars, which contains their union seems to be the only set
   useful to determine whether an identifier is prefix or infix.
   The set postfix_chars I added, which is the set of characters allowed
   at the end of an identifier to allow for internal MetaOCaml variable
   renaming. *)

let prefix_symbols  = [ '!'; '?'; '~' ]
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-';
                       '*'; '/'; '$'; '%' ]
let operator_chars = [ '!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/';
                       ':'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~' ]
let numeric_chars  = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

type fixity = | Infix | Prefix

let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="]

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string s =
  if ((List.mem s special_infix_strings)
      || (List.mem (String.get s 0) infix_symbols)) then Infix else Prefix

let fixity_of_longident = function
  | (Longident.Lident (name) | Longident.Ldot (_, name)) ->
      (fixity_of_string name) ;
  | _ -> Prefix

let fixity_of_exp e = match e.pexp_desc with
  | Pexp_ident (li) -> fixity_of_longident li.txt
  | Pexp_cspval (_,li) -> 
	  if false (* default value of !Clflags.prettycsp *)
	  then fixity_of_longident li.txt
	  else Prefix
  | _ -> Prefix

let rec fmt_longident = function
  | Longident.Lident (s) -> s
  | Longident.Ldot (Longident.Lident("Pervasives"), s) -> s
  | Longident.Ldot (y,s) -> fmt_longident y ^ "." ^ s
  | Longident.Lapply (y, z) -> fmt_longident y ^ "(" ^ (fmt_longident z) ^ ")"

let fmt_mutable_flag = function | Immutable -> "" | Mutable -> "mutable "

let fmt_virtual_flag = function | Virtual  -> "virtual " | Concrete -> ""

let fmt_ov_flag = function | Fresh -> "" | Override -> "!"

module PR(F : sig val ppf : Format.formatter end) = struct

let fmt = F.ppf
let p = pp_print_string fmt
let space () = pp_print_space fmt ()

(* useful for %a *)
let const f = fun _ x -> f x

let fenced lf rf pr e = p lf; open_box 0; pr e; close_box (); p rf 
let bp pr e = fenced "(" ")" pr e

let fmt_constant = function
  | Const_int (i) ->
      if (i < 0) then fprintf fmt "(%d)" i else fprintf fmt "%d" i
  | Const_char (c) ->
      let i = int_of_char c in
      if (i < 32) || (i >= 128) then fprintf fmt "'\\0%02x'" (Char.code c)
      else fprintf fmt "'%c'" c
  | Const_string (s) -> fprintf fmt "%S" s
  | Const_float (s) -> 
      if ((String.get s 0) = '-') then fprintf fmt "(%s)" s else p s
      (* maybe parenthesize all floats for consistency? *)
  | Const_int32 (i) -> fprintf fmt "%ld" i
  | Const_int64 (i) -> fprintf fmt "%Ld" i
  | Const_nativeint (i) -> fprintf fmt "%nd" i

let string s = p s

(* List2 - applies f to each element in list l, placing break hints
     and a separator string between the resulting outputs.          *)

let rec list2 (f:'a -> unit) ?(indent=0) ?(space=1) ?(breakfirst=false)
              ?(breaklast=false) sep l =
  let pbreak () = if (breaklast=true) then pp_print_break fmt space indent in
  match l with
    [] -> pbreak ()
  | (last::[]) -> pbreak (); f last; pbreak ()
  | (first::rest) -> 
        pbreak ();
        f first ;
        p sep;
        pp_print_break fmt space indent;
        list2 f ~indent:indent ~space:space
              ~breakfirst:false ~breaklast:breaklast sep rest

let type_var s = p ("'" ^ s)

let fmt_rec_flag = function
  | Nonrecursive -> ""
  | Recursive | Default -> " rec"
    (* todo - what is "default" recursion?? 
        this seemed safe, as it's better to falsely make a non-recursive
        let recursive than the opposite. *)

let fmt_direction_flag = function | Upto   -> "to" | Downto -> "downto"

let fmt_private_flag = function | Public -> "" | Private -> "private "

let fmt_class_params_def ppf (l, _) = match l with
  | [] -> ()
  | _  ->
    fprintf ppf "[" ;
    list2 type_var "," (List.map (fun x -> x.txt) l);
    fprintf ppf "]@ "

let fmt_class_params ppf (l, _) = match l with
  | []  -> ()
  | [x] -> fprintf ppf "%s@ " x
  | _   -> fenced "(" ")" (list2 string ",") l; space ()

let option f = function (* DELETE ?*)
  | None -> ()
  | Some x -> fprintf fmt "Some\n"; f x

let option_quiet f = function
  | None   -> ()
  | Some x -> fprintf fmt "@ " ; f x

let rec expression_is_terminal_list = function
  | {pexp_desc = Pexp_construct ({txt = Longident.Lident("[]")}, None, _)}
     -> true ;
  | {pexp_desc = Pexp_construct ({txt = Longident.Lident("::")},
                   Some({pexp_desc = Pexp_tuple([_ ; exp2])}), _)}
     -> (expression_is_terminal_list exp2)
  | {pexp_desc = _}
     -> false

(* check if it's a fixed-length list *)
let rec pat_fixed_len_list li po = match (li,po) with
    | Longident.Lident("::"), Some ({ppat_desc = Ppat_tuple([_; pat2])}) ->
        begin
          match pat2.ppat_desc with
          | Ppat_construct ({txt=li}, po, _) -> pat_fixed_len_list li po
          | _ -> false
        end
    | Longident.Lident("[]"), None -> true
    | _,_ -> false

let rec core_type x =
  match x.ptyp_desc with
  | Ptyp_any -> p "_";         (* done *)
  | Ptyp_var (s) -> fprintf fmt "'%s" s; (* done *)
  | Ptyp_arrow (l, ct1, ct2) ->          (* done *)
      pp_open_hovbox fmt indent ;
      p "(" ;
      (match l with
       | "" -> core_type ct1;
       | s when (String.get s 0 = '?')  ->
         (match ct1.ptyp_desc with
          | Ptyp_constr ({txt = Longident.Lident ("option")}, l) ->
            fprintf fmt "%s :@ " s ;
            type_constr_list fmt l ;
          | _ -> core_type ct1; (* todo: what do we do here? *)
         );
       | _ -> core_type ct1; (* todo: what do we do here? *)
      );
      fprintf fmt "@ ->@ " ;
      core_type ct2 ;
      p ")" ;
      pp_close_box fmt () ;
  | Ptyp_tuple l ->                      (* done *)
      pp_open_hovbox fmt indent ;
      p "(" ;
      list2 core_type " *" l;
      p ")" ;
      pp_close_box fmt () ;
  | Ptyp_constr (li, l) ->               (* done *)
      pp_open_hovbox fmt indent ;
      type_constr_list fmt ~space:true l ;
      p (fmt_longident li.txt);
      pp_close_box fmt () ;
  | Ptyp_object ([]) -> p "< >"
  | Ptyp_object (l) ->
      pp_open_hovbox fmt indent ;
      p "< " ;
      list2 core_field_type " ;" l;
      p " >" ;
      pp_close_box fmt ()
  | Ptyp_class (li, l, low) ->           (* done... sort of *)
      pp_open_hovbox fmt indent ;
      list2 core_type ~breaklast:true "" l;
      fprintf fmt "#%s" (fmt_longident li.txt);
      if ((List.length low) < 0) then begin (* done, untested *)
        fprintf fmt "@ [> " ;
        list2 class_var "" low;
        fprintf fmt " ]";
      end ;
      pp_close_box fmt ();
  | Ptyp_alias (ct, s) ->                (* done *)
      pp_open_hovbox fmt indent ;
      p "(" ;
      core_type ct ;
      fprintf fmt "@ as@ '%s)" s;
      pp_close_box fmt () ;
  | Ptyp_variant (l, closed, _) ->
      pp_open_hovbox fmt indent ;
      (match closed with
       | true  -> p "[ " ;
       | false -> p "[> " ;
      );
      list2 type_variant_helper " |" l;
      p " ]";
      pp_close_box fmt () ;
  | Ptyp_poly (sl, ct) ->                (* done? *)
      pp_open_hovbox fmt indent ;
      if ((List.length sl) > 0) then begin
        list2 (fprintf fmt "'%s") ~breaklast:true "" sl;
        fprintf fmt ".@ " ;
      end ;
      core_type ct ;
      pp_close_box fmt () ; 
  | Ptyp_package (li, []) ->
      p (fmt_longident li.txt)
  | Ptyp_package (li, lst) ->
      pp_open_hovbox fmt indent ;
      p (fmt_longident li.txt); 
      fprintf fmt "@ with@ ";
      list2 package_type_cstr "and" lst;
      pp_close_box fmt () ;
      failwith "Ptyp_package printing not implemented yet"

and package_type_cstr (li, typ) = 
      pp_open_hovbox fmt indent ;
      p "type ";
      p (fmt_longident li.txt);
      p " = ";
      core_type typ;
      pp_close_box fmt ()

and class_var s = fprintf fmt "`%s" s

and core_field_type x =
  match x.pfield_desc with
  | Pfield (s, ct) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "%s :@ " s;
      core_type ct;
      pp_close_box fmt () ;
  | Pfield_var ->
      p "..";

and type_constr_list ppf ?(space=false) l =
  match (List.length l) with
   | 0 -> ()
   | 1 -> list2 core_type "" l;
          if (space) then fprintf ppf " " ;
   | _ -> fprintf ppf "(" ; 
          list2 core_type "," l;
          fprintf ppf ")" ;
          if (space) then fprintf ppf " " ;

and pattern_with_label ppf x s =
  if (s = "") then pattern x
  else begin
    let s =
      if (String.get s 0 = '?') then begin
        fprintf ppf "?" ;
        String.sub s 1 ((String.length s) - 1)
      end else begin
        fprintf ppf "~" ;
        s
      end in
    p s;
    match x.ppat_desc with
    | Ppat_var (s2) ->
        if (s <> s2.txt) then begin
          fprintf ppf ":" ;
          pattern x ;
        end
    | _ -> fprintf ppf ":" ;
           pattern x
  end ;

and pattern_with_when ppf whenclause x =
  match whenclause with
  | None -> pattern x ;
  | Some (e) ->
      pp_open_hovbox ppf indent ;
      pattern x ;
      fprintf ppf "@ when@ " ;
      expression e ;
      pp_close_box ppf () ;

and pattern x =
  match x.ppat_desc with
  | Ppat_any -> p "_";            (* OXX done *)
  | Ppat_var ({txt = s}) ->
      ( match fixity_of_string s with
        | Infix  -> fprintf fmt "(%s)" s              (* OXX done *)
        | Prefix -> p s)
  | Ppat_alias (pat, {txt=s}) ->                (* OXX done ... *)
      pp_open_hovbox fmt indent ;
      p "(" ;
      pattern pat ;
      fprintf fmt " as@ %s)" s;
      pp_close_box fmt () ;
  | Ppat_constant (c) -> fmt_constant c;
  | Ppat_tuple (l) ->                       (* OXX done *)
      fprintf fmt "@[<hov 1>(";
      list2 pattern "," l;
      fprintf fmt "@])";
  | Ppat_construct ({txt=li}, po, _) -> 
      pp_open_hovbox fmt indent ;
      ( if pat_fixed_len_list li po then
        (match li,po with
         | Longident.Lident("::"),
           Some ({ppat_desc = Ppat_tuple([pat1; pat2])}) ->
             p "[" ;
             pattern pat1 ;
             pattern_finlist_helper fmt pat2 ;
             p "]"; 
         | Longident.Lident("[]"), _ -> p "[]"
         | _,_ -> assert false )
      else
        (match li,po with
         | Longident.Lident("::"),
           Some ({ppat_desc = Ppat_tuple([pat1; pat2])}) ->
             p "(" ;
             pattern pat1 ;
             fprintf fmt "@ ::@ " ;
             pattern_list_helper fmt pat2 ;
             p ")"; 
         | _,_ -> p (fmt_longident li);
            option_quiet pattern_in_parens po ) ) ;
      pp_close_box fmt () ;
      (* OXX what is the boolean at the end?  The parser always sets it
         to false, so ignore it since it can't be part of the external syntax
      *)

  | Ppat_variant (l, po) ->
      (match po with
       | None -> p l
       | Some (pat) ->
          pp_open_hovbox fmt indent ;
          fprintf fmt "(`%s@ " l ;
          pattern pat ;
          p ")" ;
          pp_close_box fmt () ;
      );
  | Ppat_record (l,cf) ->
      p "{" ;
      list2 longident_x_pattern ";" l;
      if cf = Open then fprintf fmt "; _ ";
      p "}" ;
  | Ppat_array (l) ->                      (* OXX done *)
     pp_open_hovbox fmt 2 ;
     p "[|" ;
     list2 pattern ";" l;
     p "|]" ;
     pp_close_box fmt () ;
  | Ppat_or (p1, p2) ->                    (* OXX done *)
      pp_open_hovbox fmt indent ;
      p "(" ;
      pattern p1 ;
      fprintf fmt "@ | " ;
      pattern p2 ;
      p ")" ;
      pp_close_box fmt () ;
  | Ppat_constraint (pat, ct) ->             (* OXX done, untested *)
      p "(" ;
      pattern pat ;
      p " :" ;
      pp_print_break fmt 1 indent ;
      core_type ct ;
      p ")" ;
  | Ppat_type {txt=li} ->                        (* OXX done *)
      fprintf fmt "#%s" (fmt_longident li);
  | Ppat_lazy pat ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "(lazy @ ";
      pattern pat ;
      p ")" ;
      pp_close_box fmt ()
  | Ppat_unpack {txt = s} ->
      fprintf fmt "(module @ %s)" s;

and expression x =
  match x.pexp_desc with
  | Pexp_ident ({txt=li}) -> (* was (li, b) *)
      ( match fixity_of_longident li with
        | Infix  -> fprintf fmt "(%s)" (fmt_longident li)
        | Prefix -> p (fmt_longident li))
  | Pexp_constant (c) -> fmt_constant c;
  | Pexp_let (rf, l, e) ->
      let l1 = (List.hd l) in
      let l2 = (List.tl l) in
      pp_open_hvbox fmt 0 ;
      pp_open_hvbox fmt indent ;
      fprintf fmt "let%s " (fmt_rec_flag rf);
      pattern_x_expression_def fmt l1;
      pattern_x_expression_def_list fmt l2;
      pp_close_box fmt () ;
      fprintf fmt " in@ " ;
      expression_sequence fmt ~first:false ~indent:0 e ;
      pp_close_box fmt () ;
  | Pexp_function (pat, eo, l) ->
      if (List.length l = 1) then begin
        pp_open_hvbox fmt indent;
        p "fun " ;
        pattern_x_expression_case_single fmt (List.hd l) eo pat
      end else begin
        pp_open_hvbox fmt 0;
        p "function" ;
        option_quiet expression_in_parens eo ;
        pp_print_space fmt () ;
        pattern_x_expression_case_list fmt l ;
      end ;
      pp_close_box fmt ();
  | Pexp_apply (e, l) -> (* was (e, l, _) *)
     let fixity = fixity_of_exp e in
     (* let fixity =
         (match e.pexp_desc with Pexp_ident (_, b) -> b
                                 | _ -> false ) in *)
     let sd =
       (match e.pexp_desc with
        | Pexp_ident ({txt=Longident.Ldot (Longident.Lident(modname), valname)})
          -> (modname, valname)
        | Pexp_ident ({txt=Longident.Lident(valname)})
          -> ("",valname)
        | _ -> ("","")) in 
     (match sd,l with
      | ("Array", "get"), [(_,exp1) ; (_,exp2)] ->
               pp_open_hovbox fmt indent;
               (match exp1.pexp_desc with
                | Pexp_ident (_) -> expression exp1
                | _ -> expression_in_parens exp1
               );
               p ".";
               expression_in_parens exp2;
               pp_close_box fmt ();
      | ("Array", "set"), [(_,array) ; (_,index) ; (_, value)] ->
               pp_open_hovbox fmt indent;
               (match array.pexp_desc with
                | Pexp_ident (_) -> expression array
                | _ -> expression_in_parens array
               );
               p ".";
               expression_in_parens index;
               fprintf fmt "@ <-@ ";
               expression value;
               pp_close_box fmt ()
      | ("","!"),[(_,exp1)] ->
               p "!" ;
               expression exp1
      (* | ("","raise"),[(_,exp)] ->
               p "raising [" ;
               expression exp;
               fprintf fmt "], says %s" st; *)
      | _,_ -> begin
          pp_open_hovbox fmt (indent + 1) ; 
          p "(" ;
          ( match fixity with
          | Prefix -> begin        
             (match e.pexp_desc with
              | Pexp_ident(_) -> expression e ;
              | Pexp_send (_,_) -> expression e ;
              | _ -> pp_open_hovbox fmt indent;
                     expression_in_parens e ;
                     pp_close_box fmt () );
             fprintf fmt "@ " ;
             list2 label_x_expression_param "" l
            end
          | Infix -> begin
              let args = (List.length l) in
              if (args = 2) then begin
                let arg1 = (List.hd l) in
                let arg2 = (List.hd (List.tl l)) in
                label_x_expression_param arg1 ;
                pp_print_space fmt () ;
                (match e.pexp_desc with
                 | Pexp_ident({txt = li}) ->
                     (* override parenthesization of infix identifier *)
                     p (fmt_longident li);
                 | _ -> expression e) ;
                pp_print_space fmt () ;
                label_x_expression_param arg2 
              end
              else begin
                (* p "(" ; *)
                expression e ;
                (* p ")" ; *)
                list2 label_x_expression_param ~breakfirst:true "" l
              end
            end ) ;
          p ")" ;
          pp_close_box fmt ()
          end )
  | Pexp_match (e, l) ->
      p "(" ;
      pp_open_hvbox fmt 0;
      pp_open_hovbox fmt 2;
      fprintf fmt "match@ " ;
      expression e ;
      p " with" ;
      pp_close_box fmt () ;
      pp_print_space fmt () ;
      pattern_x_expression_case_list fmt l ;
      pp_close_box fmt () ;
      p ")" ;
  | Pexp_try (e, l) ->
      p "(";
      pp_open_vbox fmt 0; (* <-- always break here, says style manual *)
      pp_open_hvbox fmt 0;
      p "try";
      pp_print_break fmt 1 indent ;
      expression_sequence fmt ~first:false e;
      pp_print_break fmt 1 0;
      p "with";
      pp_close_box fmt ();
      pp_print_cut fmt ();
      pattern_x_expression_case_list fmt l ;
      pp_close_box fmt ();
      p ")";
  | Pexp_tuple (l) ->
      fprintf fmt "@[<hov 1>(";
      list2 (bp expression) "," l;
      fprintf fmt ")@]";
  | Pexp_construct ({txt=li}, None, _) -> p (fmt_longident li)
  | Pexp_construct ({txt=li}, Some eo, _) ->
      (match li with
       | Longident.Lident ("::") ->
           (match eo with
              ({pexp_desc = Pexp_tuple ([exp1 ; exp2])}) ->
               pp_open_hovbox fmt indent ;
               if (expression_is_terminal_list exp2) then begin
                 p "[" ;
                 expression exp1 ;
                 expression_list_helper fmt exp2 ;
                 p "]" ;
               end else begin
                 pp_open_hovbox fmt indent ;
                 p "(" ;
                 expression exp1 ;
                 fprintf fmt " ::@ " ;
                 expression_list_nonterminal fmt exp2 ;
                 p ")" ;
                 pp_close_box fmt () ;
               end ;
               pp_close_box fmt () ;
            | _ -> assert false
           );
       | _ -> 
           pp_open_hovbox fmt indent ;
           p (fmt_longident li);
           expression_in_parens eo;
           pp_close_box fmt () ;
           p ")"
      );
  | Pexp_variant (l, eo) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "`%s" l ;
      option_quiet expression eo ;
      pp_close_box fmt () ;
  | Pexp_record (l, None) ->
      pp_open_hovbox fmt indent ; (* maybe just 1? *)
      p "{" ;
      list2 longident_x_expression ";" l;
      p "}" ;
      pp_close_box fmt () ;
  | Pexp_record (l, Some eo) ->
      pp_open_hovbox fmt indent ; (* maybe just 1? *)
      p "{" ;
      expression eo;
      p " with ";
      list2 longident_x_expression ";" l;
      p "}" ;
      pp_close_box fmt () ;
  | Pexp_field (e, {txt=li}) ->
      pp_open_hovbox fmt indent ;
      (match e.pexp_desc with
       | Pexp_ident (_) ->
           expression e ;
       | _ ->
           expression_in_parens e ;
      );
      fprintf fmt ".%s" (fmt_longident li);
      pp_close_box fmt () ;
  | Pexp_setfield (e1, {txt=li}, e2) ->
      pp_open_hovbox fmt indent ;
      (match e1.pexp_desc with
       | Pexp_ident (_) ->
           expression e1 ;
       | _ ->
           expression_in_parens e1 ;
      );
      fprintf fmt ".%s" (fmt_longident li);
      fprintf fmt "@ <-@ ";
      expression e2;
      pp_close_box fmt () ;
  | Pexp_array (l) ->
      pp_open_hovbox fmt 2 ;
      p "[|" ;
      list2 expression ";" l;
      p "|]" ;
      pp_close_box fmt () ;
  | Pexp_ifthenelse (e1, e2, eo) ->
      fprintf fmt "@[<hv 0>" ;
      expression_if_common fmt e1 e2 eo;
      fprintf fmt "@]";

  | Pexp_sequence (_, _) ->
      fprintf fmt "@[<hv 0>begin" ;
      pp_print_break fmt 1 indent ;
      (* "@;<1 2>" ; *)
      expression_sequence fmt ~first:false x ;
      fprintf fmt "@;<1 0>end@]" ;
  | Pexp_while (e1, e2) ->
      pp_open_hvbox  fmt 0 ;
      pp_open_hovbox fmt indent ;
      fprintf fmt "while@ " ;
      expression e1 ;
      p " do" ;
      pp_close_box fmt () ;
      pp_print_break fmt 1 indent ;
      expression_sequence fmt e2 ~first:false;
      pp_print_break fmt 1 0 ;
      p "done" ;
      pp_close_box fmt () ;
  | Pexp_for ({txt=s}, e1, e2, df, e3) ->
      pp_open_hvbox  fmt 0 ;
      pp_open_hovbox fmt indent ;
      fprintf fmt "for %s =@ " s ;
      expression e1 ;
      fprintf fmt "@ %s@ " (fmt_direction_flag df);
      expression e2 ;
      p " do" ;
      pp_close_box fmt () ;

      pp_print_break fmt 1 indent ;
      expression_sequence fmt ~first:false e3 ;
      pp_print_break fmt 1 0 ;
      p "done" ;
      pp_close_box fmt () ;
  | Pexp_constraint (e, cto1, cto2) ->
      pconstraint e cto1 cto2
  | Pexp_when (_, _) ->
      assert false ;
      (* This is a weird setup. The ocaml phrase
          "pattern when condition -> expression"
          found in pattern matching contexts is encoded as:
          "pattern -> when condition expression"
         Thus, the when clause ("when condition"), which one might expect
          to be part of the pattern, is encoded as part of the expression
          following the pattern.
         A "when clause" should never exist in a vaccum. It should always
          occur in a pattern matching context and be printed as part of the
          pattern (in pattern_x_expression_case_list).
         Thus these Pexp_when expressions are printed elsewhere, and if
          this code is executed, an error has occurred. *)
  | Pexp_send (e, s) ->
      pp_open_hovbox fmt indent;
      (match e.pexp_desc with
       | Pexp_ident(_) ->
           expression e;
           fprintf fmt "#%s" s;
       | _ ->
           fprintf fmt "(" ;
           expression_in_parens e;
           fprintf fmt "@,#%s" s;
           fprintf fmt ")"
      );
      pp_close_box fmt (); (* bug fixed? *)
  | Pexp_new {txt=li} ->
      pp_open_hovbox fmt indent;
      fprintf fmt "new@ %s" (fmt_longident li);
      pp_close_box fmt ();
  | Pexp_setinstvar ({txt=s}, e) ->
      pp_open_hovbox fmt indent;
      fprintf fmt "%s <-@ " s;
      expression e;
      pp_close_box fmt ();
  | Pexp_override (l) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "{< " ;
      if ((List.length l) > 0) then begin
        list2 string_x_expression ";" l;
        fprintf fmt " " ;
      end ;
      fprintf fmt ">}" ;
      pp_close_box fmt () ;
  | Pexp_letmodule ({txt=s}, me, e) ->
      pp_open_hvbox fmt 0 ;
      pp_open_hovbox fmt indent ;
      fprintf fmt "let module %s =@ " s ;
      module_expr fmt me ;
      fprintf fmt " in" ;
      pp_close_box fmt () ;
      pp_print_space fmt () ;
      expression_sequence fmt ~first:false ~indent:0 e ;
      pp_close_box fmt () ;
  | Pexp_assert (e) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "assert@ " ;
      expression e ;
      pp_close_box fmt () ;
  | Pexp_assertfalse ->
      fprintf fmt "assert false" ;
  | Pexp_lazy (e) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "lazy@ " ;
      expression e ;
      pp_close_box fmt () ;
  | Pexp_poly (e, cto) ->
      (* should this even print by itself? *)
      (match cto with
       | None -> expression e ;
       | Some (ct) ->
          pp_open_hovbox fmt indent ;
          expression e ;
          fprintf fmt "@ (* poly:@ " ;
          core_type ct ;
          fprintf fmt " *)" ;
          pp_close_box fmt () );
  | Pexp_object cs ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "object@ " ;
      class_structure fmt cs ;
      pp_close_box fmt () ;
  | Pexp_newtype (li,fun_def) -> (* XXX may sometimes need a leading 'fun'? *)
      pp_open_hovbox fmt indent ;
      fprintf fmt "(type %s) %a" li (const expression) fun_def;
      pp_close_box fmt ()
  | Pexp_pack me ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "(module %a)" module_expr me;
      pp_close_box fmt ()
  | Pexp_open (li, sexpr) -> 
      fprintf fmt "let open %s in@ " (fmt_longident li.txt);
      pp_open_hovbox fmt indent ;
      expression_sequence fmt sexpr;
      pp_close_box fmt ()

(* XXO *)
  | Pexp_bracket (e) ->
      pp_open_hovbox fmt 2 ; (* indent ? *)
      fprintf fmt ".<" ;
      expression e;
      fprintf fmt ">." ;
      pp_close_box fmt () ;
  | Pexp_escape (e) ->
      fprintf fmt ".~";
      (match e.pexp_desc with
       | Pexp_ident (_) ->
          expression e;
       | _ ->
          expression_in_parens e;
      );
  | Pexp_run (e) ->
      fprintf fmt ".!";
      (match e.pexp_desc with
         Pexp_bracket (e') ->
           expression e;
       | _ ->
           expression_in_parens e)
  | Pexp_cspval (v,loc) ->
      if false (* default value of (!Clflags.prettycsp)  *)
      then p (fmt_longident loc.txt)
      (* was: compiled code omitted
         however, CSP values's are not always compiled code:
           let f x = x in
           let g   = .!.<fun x -> f x>. *)
      else 
          fprintf fmt "(* cross-stage persistent value (id: %s) *)"
               (fmt_longident loc.txt)

and value_description ppf x =
  pp_open_hovbox ppf indent ;
  core_type x.pval_type;
  if ((List.length x.pval_prim) > 0) then begin
    fprintf ppf " =@ " ;
    list2 string "" x.pval_prim
  end ;
  pp_close_box ppf () ;

and type_declaration ppf x =
  pp_open_hovbox ppf indent ;
  (match x.ptype_kind with
   | Ptype_variant (first::rest) ->
      pp_open_hovbox ppf indent ;
      pp_open_hvbox ppf 0 ;
      type_variant_leaf ppf first true ;
      type_variant_leaf_list ppf rest ;
      (* string_x_core_type_list ppf lst; *)
      pp_close_box ppf () ;
      pp_close_box ppf () ;
   | Ptype_variant [] ->
      assert false ;
   | Ptype_abstract ->
      (match x.ptype_manifest with
       | None -> ()
       | Some(y) -> core_type y)
   | Ptype_record l -> 
       pp_open_hovbox ppf indent ;

       fprintf ppf "{" ;
       pp_print_break ppf 0 indent ;
       pp_open_hvbox ppf 0;
       list2 type_record_field ";" l;
       pp_close_box ppf () ;
       fprintf ppf "@," ;
       pp_close_box ppf () ;
       fprintf ppf "}" ;

       pp_close_box ppf () ;
  );
  list2 typedef_constraint ~breakfirst:true "" x.ptype_cstrs;
  pp_close_box ppf ()

and pconstraint ?(simple=true) e cto1 cto2 =
    pp_open_hovbox fmt 2 ;
    if simple then p "(";
    expression e;
    (match cto1 with
     | Some x1 -> fprintf fmt " :@ " ; core_type x1
     | None    -> () ) ;
    (match cto2 with
     | Some x2 -> fprintf fmt " :>@ " ; core_type x2
     | None    -> () ) ;
    if simple then p ")";
    pp_close_box fmt ()

and exception_declaration ppf x =
  match x with
  | [] -> ()
  | first::rest ->
      fprintf ppf "@ of@ ";
      list2 core_type " *" x;

and class_type ppf x =
  match x.pcty_desc with
  | Pcty_signature (cs) ->
      class_signature ppf cs;
  | Pcty_constr ({txt=li}, l) ->
      pp_open_hovbox ppf indent ;
      (match l with
       | [] -> ()
       | _  -> fprintf ppf "[" ;
               list2 core_type "," l;
               fprintf ppf "]@ " ); 
      p (fmt_longident li);
      pp_close_box ppf () ;
  | Pcty_fun (l, co, cl) ->
      pp_open_hovbox ppf indent ;
      core_type co ;
      fprintf ppf " ->@ " ;
      (match l with
       | "" -> () ;
       | _  -> fprintf ppf "[%s] " l ); (* todo - what's l *)
      class_type ppf cl ;
      pp_close_box ppf () ;

and class_signature ppf {pcsig_self=ct; pcsig_fields=l} =
  pp_open_hvbox ppf 0;
  pp_open_hovbox ppf indent ;
  fprintf ppf "object";
  (match ct.ptyp_desc with
   | Ptyp_any -> ()
   | _ -> fprintf ppf "@ (";
          core_type ct;
          fprintf ppf ")" );
  pp_close_box ppf () ;
  list2 class_type_field ~indent:indent ~breakfirst:true "" l;
  pp_print_break ppf 1 0;
  fprintf ppf "end";

and class_type_field {pctf_desc=x} =
  match x with
  | Pctf_inher (ct) ->      (* todo: test this *)
      pp_open_hovbox fmt indent ;
      fprintf fmt "inherit@ " ;
      class_type fmt ct ;
      pp_close_box fmt () ;
  | Pctf_val (s, mf, vf, ct) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "val %s%s%s :@ " (fmt_mutable_flag mf) 
        (fmt_virtual_flag vf) s;
      core_type ct ;
      pp_close_box fmt () ;
  | Pctf_virt (s, pf, ct) ->    (* todo: test this *)
      pp_open_hovbox fmt indent ;
      pp_open_hovbox fmt indent ;
      fprintf fmt "method@ %svirtual@ %s" (fmt_private_flag pf) s ;
      pp_close_box fmt () ;
      fprintf fmt " :@ " ;
      core_type ct ;
      pp_close_box fmt () ;
  | Pctf_meth (s, pf, ct) ->
      pp_open_hovbox fmt indent ;
      pp_open_hovbox fmt indent ;
      fprintf fmt "method %s%s" (fmt_private_flag pf) s;
      pp_close_box fmt () ;
      fprintf fmt " :@ " ;
      core_type ct ;
      pp_close_box fmt () ;
  | Pctf_cstr (ct1, ct2) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "constraint@ " ;
      core_type ct1;
      fprintf fmt " =@ " ;
      core_type ct2;
      pp_close_box fmt () ;

and class_description x =
  pp_open_hvbox fmt 0 ;
  pp_open_hovbox fmt indent ;
  fprintf fmt "class %s%a%s :" (fmt_virtual_flag x.pci_virt)
    fmt_class_params_def x.pci_params x.pci_name.txt ; 
  pp_close_box fmt () ;
  pp_print_break fmt 1 indent ;
  class_type fmt x.pci_expr ;
  pp_close_box fmt () ;

and class_type_declaration ppf x =
  class_type_declaration_ext ppf true x ;

and class_type_declaration_ext ppf first x =
  pp_open_hvbox ppf 0;
  pp_open_hovbox ppf indent ;
  fprintf ppf "%s@ %s%a%s =" (if (first) then "class type" else "and")
    (fmt_virtual_flag x.pci_virt) fmt_class_params_def x.pci_params
    x.pci_name.txt ; 
  pp_close_box ppf ();
  pp_print_break ppf 1 indent ;
  class_type ppf x.pci_expr;
  pp_close_box ppf ();

and class_type_declaration_list ppf ?(first=true) l =
  if (first) then pp_open_hvbox ppf 0 ;
  match l with
  | [] -> if (first) then pp_close_box ppf () ;
  | h :: [] ->
      class_type_declaration_ext ppf first h ;
      pp_close_box ppf () ;
  | h :: t ->
      class_type_declaration_ext ppf first h ;
      pp_print_space ppf () ;
      class_type_declaration_list ppf ~first:false t ;

and class_expr ppf x =
 match x.pcl_desc with
 | Pcl_structure (cs) ->
     class_structure ppf cs ;
 | Pcl_fun (l, eo, pat, e) ->
     pp_open_hvbox ppf indent;
     pp_open_hovbox ppf indent;
     fprintf ppf "fun@ ";
     pattern pat;
     fprintf ppf " ->";
     pp_close_box ppf ();
     (match (eo, l) with
      | (None, "") -> () ;
      | (_,_) ->
          pp_open_hovbox ppf indent;
          fprintf ppf " (* eo: ";
          option expression eo;
          fprintf ppf "@ label: ";
          p l;
          fprintf ppf " *)";
          pp_close_box ppf ()
     );
     fprintf ppf "@ ";
     class_expr ppf e;
     pp_close_box ppf ();
 | Pcl_let (rf, l, ce) ->
     let l1 = (List.hd l) in
     let l2 = (List.tl l) in
     pp_open_hvbox ppf 0 ;
     pp_open_hvbox ppf indent ;
     fprintf ppf "let%s " (fmt_rec_flag rf);
     pattern_x_expression_def ppf l1;
     pattern_x_expression_def_list ppf l2;
     pp_close_box ppf () ;
     pp_close_box ppf () ;
     fprintf ppf " in@ " ;
     class_expr ppf ce;
 | Pcl_apply (ce, l) ->
     pp_open_hovbox ppf indent ;
     fprintf ppf "(";
     class_expr ppf ce;
     list2 label_x_expression_param ~breakfirst:true "" l;
     fprintf ppf ")";
     pp_close_box ppf () ;
 | Pcl_constr ({txt=li}, l) ->
     pp_open_hovbox ppf indent;
     if ((List.length l) != 0) then begin
       fprintf ppf "[" ;
       list2 core_type "," l;
       fprintf ppf "]@ " ;
     end ;
     p (fmt_longident li);
     pp_close_box ppf ();
 | Pcl_constraint (ce, ct) ->
     pp_open_hovbox ppf indent;
     fprintf ppf "(";
     class_expr ppf ce;
     fprintf ppf "@ : ";
     class_type ppf ct;
     fprintf ppf ")";
     pp_close_box ppf ();

and class_structure ppf {pcstr_pat=p; pcstr_fields=l} =
  pp_open_hvbox ppf 0 ;
  pp_open_hovbox ppf indent ;
  fprintf ppf "object" ;
  (match p.ppat_desc with
   | Ppat_any -> ();
   | _ -> fprintf ppf "@ " ;
          pattern_in_parens p );
  pp_close_box ppf () ;
  list2 class_field ~indent:indent ~breakfirst:true "" l;
  fprintf ppf "@ end" ;
  pp_close_box ppf () ;

and class_field {pcf_desc=x} =
  match x with
  | Pcf_inher (ov,ce, so) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "inherit@ %s" (fmt_ov_flag ov);
      class_expr fmt ce;
      (match so with
       | None -> ();
       | Some (s) -> fprintf fmt "@ as %s" s );
      pp_close_box fmt ();
  | Pcf_valvirt ({txt=s}, mf, ct) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "val virtual %s%s" (fmt_mutable_flag mf) s;
      fprintf fmt " :@ " ;
      core_type ct;
      pp_close_box fmt () ;
  | Pcf_val ({txt=s}, mf, ov, e) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "val %s%s%s =@ " (fmt_ov_flag ov) (fmt_mutable_flag mf) s ;
      expression_sequence fmt ~indent:0 e ;
      pp_close_box fmt () ;
  | Pcf_virt ({txt=s}, pf, ct) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "method virtual %s%s" (fmt_private_flag pf) s ;
      fprintf fmt " :@ " ;
      core_type ct;
      pp_close_box fmt () ;
  | Pcf_meth ({txt=s}, pf, ov, e) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "method %s%s%s" (fmt_ov_flag ov) (fmt_private_flag pf) s ;
      (match e.pexp_desc with
       | Pexp_poly (e, Some(ct)) ->
           fprintf fmt " :@ " ;
           core_type ct ;
           fprintf fmt " =@ " ;
           expression e ;
       | _ -> 
           fprintf fmt " =@ " ;
           expression e;
      ) ;
      (* special Pexp_poly handling? *)
      pp_close_box fmt () ;
  | Pcf_constr (ct1, ct2) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "constraint@ ";
      core_type ct1;
      fprintf fmt " =@ " ;
      core_type ct2;
      pp_close_box fmt ();
  | Pcf_init (e) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "initializer@ " ;
      expression_sequence fmt ~indent:0 e ;
      pp_close_box fmt () ;

and class_fun_helper ppf e =
  match e.pcl_desc with
  | Pcl_fun (l, eo, pat, e) ->
      pattern pat;
      fprintf ppf "@ ";
      (match (eo, l) with
       | (None, "") -> () ;
       | (_,_) ->
           fprintf ppf "(* ";
           option expression eo;
           p l;
           fprintf ppf " *)@ "
      );
      class_fun_helper ppf e;
  | _ -> e;

and class_declaration_list ppf ?(first=true) l =
  match l with
  | [] -> if (first = false) then pp_close_box ppf ();
  | cd::l ->
      let s = (if first then begin pp_open_hvbox ppf 0 ; "class" end
               else begin "@ and" end) in
      class_declaration ppf ~str:s cd ;
      class_declaration_list ppf ~first:false l ;

and class_declaration ppf ?(str="class") x =
  pp_open_hvbox ppf indent ;
  pp_open_hovbox ppf indent ;
  fprintf ppf "%s %s%a%s@ " str (fmt_virtual_flag x.pci_virt)
    fmt_class_params_def x.pci_params x.pci_name.txt ;
  let ce =
    (match x.pci_expr.pcl_desc with
     | Pcl_fun (l, eo, p, e) ->
        class_fun_helper ppf x.pci_expr;
     | _ -> x.pci_expr) in
  let ce =
    (match ce.pcl_desc with
     | Pcl_constraint (ce, ct) ->
         fprintf ppf ":@ " ;
         class_type ppf ct ;
         fprintf ppf "@ " ;
         ce
     | _ -> ce ) in
  fprintf ppf "=" ;
  pp_close_box ppf () ;
  fprintf ppf "@ " ;
  class_expr ppf ce ;
  pp_close_box ppf () ;

and module_type ppf x =
  match x.pmty_desc with
  | Pmty_ident ({txt=li}) ->
      p (fmt_longident li);
  | Pmty_signature (s) ->
      pp_open_hvbox ppf 0;
      fprintf ppf "sig";
      list2 signature_item ~breakfirst:true ~indent:indent "" s;
      pp_print_break ppf 1 0;
      fprintf ppf "end";
      pp_close_box ppf ();
  | Pmty_functor ({txt=s}, mt1, mt2) ->
      pp_open_hvbox ppf indent;
      pp_open_hovbox ppf indent;
      fprintf ppf "functor@ (%s : " s ;
      module_type ppf mt1;
      fprintf ppf ") ->";
      pp_close_box ppf ();
      pp_print_space ppf ();
      module_type ppf mt2;
      pp_close_box ppf ();
  | Pmty_with (mt, l) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "(" ;
      module_type ppf mt ;
      fprintf ppf "@ with@ " ;
      with_constraint_list ppf l ;
      fprintf ppf ")" ;
      pp_close_box ppf ()
  | Pmty_typeof me -> 
      pp_open_hovbox ppf 0 ;
      fprintf fmt "module type of@ %a" module_expr me;
      pp_close_box ppf ()

and signature x = List.iter signature_item x

and signature_item x =
  match x.psig_desc with
  | Psig_type (l) ->
      let first = (List.hd l) in
      let rest  = (List.tl l) in
      pp_open_hvbox fmt 0;
      pp_open_hvbox fmt 0;
      fprintf fmt "type " ;
      string_x_type_declaration fmt first;
      pp_close_box fmt ();
      type_def_list_helper fmt rest;
      pp_close_box fmt ();
  | Psig_value ({txt=s}, vd) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "val %s :@ " s;
      value_description fmt vd;
      pp_close_box fmt () ;
  | Psig_exception ({txt=s}, ed) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "exception %s" s;
      exception_declaration fmt ed;
      pp_close_box fmt ();
  | Psig_class (l) ->
      pp_open_hvbox fmt 0 ;
      list2 class_description "" l;
      pp_close_box fmt () ;
  | Psig_module ({txt=s}, mt) ->  (* todo: check this *)
      pp_open_hovbox fmt indent ;
      pp_open_hovbox fmt indent ;
      fprintf fmt "module@ %s =" s ;
      pp_close_box fmt () ;
      pp_print_space fmt () ;
      module_type fmt mt;
      pp_close_box fmt () ;
  | Psig_open ({txt=li}) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "open@ %s" (fmt_longident li);
      pp_close_box fmt () ;
  | Psig_include (mt) ->  (* todo: check this *)
      pp_open_hovbox fmt indent ;
      fprintf fmt "include@ " ;
      module_type fmt mt;
      pp_close_box fmt () ;
  | Psig_modtype ({txt=s}, md) -> (* todo: check this *)
      pp_open_hovbox fmt indent ;
      fprintf fmt "module type %s" s ;
      (match md with
       | Pmodtype_abstract -> ()
       | Pmodtype_manifest (mt) ->
           pp_print_space fmt () ;
           module_type fmt mt;
      );
      pp_close_box fmt () ;
  | Psig_class_type (l) ->
      class_type_declaration_list fmt l ;
  | Psig_recmodule decls ->
      pp_open_hvbox fmt 0 ;
      pp_open_hovbox fmt indent ;
      fprintf fmt "module rec@ " ;
      string_x_module_type_list fmt decls ; (* closes hov box *)
      pp_close_box fmt () ;

and modtype_declaration ppf x =
  match x with
  | Pmodtype_abstract -> fprintf ppf "Pmodtype_abstract\n";
  | Pmodtype_manifest (mt) ->
      fprintf ppf "Pmodtype_manifest\n";
      module_type ppf mt;

and module_expr ppf x =
  match x.pmod_desc with
  | Pmod_ident ({txt=li}) ->
      p (fmt_longident li);
  | Pmod_structure (s) ->
      pp_open_hvbox ppf 0;
      fprintf ppf "struct";
      list2 structure_item ~breakfirst:true ~indent:indent "" s;
      pp_print_break ppf 1 0;
      fprintf ppf "end";
      pp_close_box ppf (); (* bug fixed? *)
  | Pmod_functor ({txt=s}, mt, me) ->
      pp_open_hvbox ppf indent ;
      fprintf ppf "functor (%s : " s;
      module_type ppf mt;
      fprintf ppf ") ->@ ";
      module_expr ppf me;
      pp_close_box ppf () ;
  | Pmod_apply (me1, me2) ->
      pp_open_hovbox ppf indent;
      module_expr ppf me1;
      pp_print_cut ppf ();
      fprintf ppf "(" ;
      module_expr ppf me2;
      fprintf ppf ")" ;
      pp_close_box ppf ();
  | Pmod_constraint (me, mt) ->
      fprintf ppf "(";
      pp_open_hovbox ppf indent;
      module_expr ppf me;
      fprintf ppf " :@ ";  (* <-- incorrect indentation? *)
      module_type ppf mt;
      pp_close_box ppf ();
      fprintf ppf ")";
  | Pmod_unpack e ->
      pp_open_hovbox ppf indent;
      p "( val";
      ( match e.pexp_desc with
        | Pexp_constraint (ex, ct1, ct2) -> pconstraint ~simple:false ex ct1 ct2
        | _                              -> expression e);
      p ")";
      pp_close_box ppf ()

and structure x = List.iter structure_item x

(* closes one box *)
and string_x_modtype_x_module ppf ({txt=s}, mt, me) =
(*
  (match me.pmod_desc with
   | Pmod_constraint (me, ({pmty_desc=(Pmty_ident (_)
        | Pmty_signature (_))} as mt)) ->
       (* assert false ; *) (* 3.07 - should this ever happen here? *)
       fprintf ppf "%s :@ " s ;
       module_type ppf mt ;
       fprintf ppf " =" ;
       pp_close_box ppf () ;
       pp_print_space ppf () ;
       module_expr ppf me ;
   | _ ->
*)
       fprintf ppf "%s :@ " s;
       module_type ppf mt ;
       fprintf ppf " =" ;
       pp_close_box ppf () ;
       pp_print_space ppf () ;
       module_expr ppf me ;
(*  ) ; *)

(* net gain of one box (-1, +2) *)
and string_x_modtype_x_module_list ppf l =
  match l with
  | [] -> ()
  | hd :: tl ->
          pp_close_box ppf () ;
          pp_print_space ppf () ;
          pp_open_hvbox ppf indent ;
          pp_open_hovbox ppf indent ;
          fprintf ppf "and " ;
          string_x_modtype_x_module ppf hd; (* closes a box *)
          string_x_modtype_x_module_list ppf tl ; (* net open of one box *)

(* context: [hv [hov .]]  returns [hv .]
   closes inner hov box. *)
and string_x_module_type_list ppf ?(first=true) l = 
  match l with
  | [] -> () ;
  | hd :: tl ->
     if (first=false) then begin
       pp_print_space ppf () ;
       pp_open_hovbox ppf indent ;
       fprintf ppf "and " ;
     end ;
     string_x_module_type ppf hd ;
     pp_close_box ppf () ;
     string_x_module_type_list ppf ~first:false tl ;
     
and string_x_module_type ppf ({txt=s}, mty) =
  fprintf ppf "%s :@ " s ;
  module_type ppf mty ;

and structure_item x =
  match x.pstr_desc with
  | Pstr_eval (e) -> 
      pp_open_hvbox fmt 0 ;
      expression_sequence fmt ~first:false ~indent:0 e ;
      pp_close_box fmt () ;
  | Pstr_type (l) ->
      let first = (List.hd l) in
      let rest  = (List.tl l) in
      pp_open_vbox fmt 0;
      pp_open_hvbox fmt 0;
      fprintf fmt "type " ;
      string_x_type_declaration fmt first;
      pp_close_box fmt ();
      type_def_list_helper fmt rest;
      pp_close_box fmt ();
  | Pstr_value (rf, l) -> 
      let l1 = (List.hd l) in
      let l2 = (List.tl l) in
      pp_open_hvbox fmt 0 ;
      pp_open_hvbox fmt indent ;
      fprintf fmt "let%s " (fmt_rec_flag rf);
      pattern_x_expression_def fmt l1;
      pattern_x_expression_def_list fmt l2;
      pp_close_box fmt () ;
      pp_close_box fmt () ;
  | Pstr_exception ({txt=s}, ed) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "exception@ %s" s;
      exception_declaration fmt ed;
      pp_close_box fmt () ;
  | Pstr_module ({txt=s}, me) ->
      pp_open_hvbox fmt indent;
      pp_open_hovbox fmt indent ;
      fprintf fmt "module %s" s ;
      (match me.pmod_desc with
       | Pmod_constraint (me, ({pmty_desc=(Pmty_ident (_)
            | Pmty_signature (_))} as mt)) ->
           fprintf fmt " :@ " ;
           module_type fmt mt ;
           fprintf fmt " =" ;
           pp_close_box fmt () ;
           pp_print_space fmt () ;
           module_expr fmt me ;
       | _ ->
           fprintf fmt " =" ;
           pp_close_box fmt () ;
           pp_print_space fmt () ;
           module_expr fmt me ;
      ) ;
      pp_close_box fmt ();
  | Pstr_open ({txt=li}) ->
      fprintf fmt "open %s" (fmt_longident li);
  | Pstr_modtype ({txt=s}, mt) ->
      pp_open_hovbox fmt indent;
      fprintf fmt "module type %s =@ " s;      
      module_type fmt mt;
      pp_close_box fmt () ; (* bug fixed? *)
  | Pstr_class (l) ->
      class_declaration_list fmt l;
  | Pstr_class_type (l) ->
      class_type_declaration_list fmt l ;
  | Pstr_primitive ({txt=s}, vd) ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "external@ %s :@ " s;
      value_description fmt vd;
      pp_close_box fmt () ;
  | Pstr_include me ->
      pp_open_hovbox fmt indent ;
      fprintf fmt "include " ;
      module_expr fmt me ;
      pp_close_box fmt () ;
  | Pstr_exn_rebind ({txt=s}, {txt=li}) ->        (* todo: check this *)
      pp_open_hovbox fmt indent ;
      fprintf fmt "exception@ %s =@ %s" s (fmt_longident li);
      pp_close_box fmt () ;
  | Pstr_recmodule decls -> (* 3.07 *)
      let l1 = (List.hd decls) in
      let l2 = (List.tl decls) in
      pp_open_hvbox fmt 0;        (* whole recmodule box *)
      pp_open_hvbox fmt indent ;  (* this definition box *)
      pp_open_hovbox fmt indent ; (* first line box *)
      fprintf fmt "module rec " ;
      string_x_modtype_x_module fmt l1; (* closes a box *)
      string_x_modtype_x_module_list fmt l2; (* net opens one box *)
      pp_close_box fmt () ;
      pp_close_box fmt () ;
      pp_close_box fmt () ;

and type_def_list_helper ppf l =
  match l with
  | [] -> ()
  | first :: rest ->
      pp_print_space ppf () ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "and " ;
      string_x_type_declaration ppf first;
      pp_close_box ppf () ;
      type_def_list_helper ppf rest ;

and string_x_type_declaration ppf ({txt=s}, td) =
  let l = List.map (function None -> "" | Some{txt=x} -> x)
                   td.ptype_params in
  (match l with
    | [] -> ()
    | [_] -> list2 type_var "" l;
             fprintf ppf " " ;
    | _ -> pp_open_hovbox ppf indent ;
           fprintf ppf "(" ; 
           list2 type_var "," l;
           fprintf ppf ")" ;
           pp_close_box ppf ();
           fprintf ppf " " ;
  );
  p s ;
  (match (td.ptype_kind, td.ptype_manifest) with
    | Ptype_abstract, None -> ()
    | Ptype_record _, _ -> fprintf ppf " = " ;
    | _ , _ -> fprintf ppf " =" ;
               pp_print_break ppf 1 indent ;
  );
  type_declaration ppf td;

and with_constraint_list ?(first=true) ppf = function
  | [] -> () ;
  | h :: [] ->
     if (first = false) then fprintf ppf "@ and " ;
     with_constraint ppf h ;
  | h :: t  ->
     if (first = false) then fprintf ppf "@ and " ;
     with_constraint ppf h ;
     fprintf ppf "@ and " ;
     with_constraint ppf h ;
     with_constraint_list ~first:false ppf t;

and with_constraint ppf ({txt=li}, wc) = match wc with
  | Pwith_type (td) ->
      fprintf ppf "type@ %s =@ " (fmt_longident li);
      type_declaration ppf td ;
  | Pwith_module ({txt=li2}) ->
      fprintf ppf "module %s =@ %s" (fmt_longident li) (fmt_longident li2);
  | Pwith_typesubst _
  | Pwith_modsubst _ ->                 (* XXX Notdone *)
      failwith "Pwithtypesubst and Pwith_modsubst aren't printed yet"

and typedef_constraint (ct1, ct2, l) =
  pp_open_hovbox fmt indent ;
  fprintf fmt "constraint@ " ; 
  core_type ct1;
  fprintf fmt " =@ " ;
  core_type ct2;
  pp_close_box fmt () ;

and type_variant_leaf ppf ({txt=s}, l, vo,_) first = (* XXX Notdone: vo *)
  if (first) then begin
    pp_print_if_newline ppf ();
    pp_print_string ppf "  ";
  end else begin
    fprintf ppf "@ | " ;
  end ;
  pp_open_hovbox ppf indent ;
  p s ;
  if ((List.length l) > 0) then begin
    fprintf ppf "@ of@ " ;
    list2 core_type " *" l
  end ;
  pp_close_box ppf ();

and type_variant_leaf_list ppf list =
  match list with
  | [] -> ()
  | first :: rest ->
    type_variant_leaf ppf first false ;
    type_variant_leaf_list ppf rest ;

and type_record_field ({txt=s}, mf, ct,_) =
  pp_open_hovbox fmt indent ;
  fprintf fmt "%s%s:" (fmt_mutable_flag mf) s ;
  core_type ct ;
  pp_close_box fmt () ;

and longident_x_pattern ({txt=li}, pat) =
  pp_open_hovbox fmt indent ;
  fprintf fmt "%s =@ " (fmt_longident li);
  pattern pat;
  pp_close_box fmt () ;

and pattern_x_expression_case_list
 ppf ?(first:bool=true) ?(special_first_case=bar_on_first_case)
 (l:(pattern * expression) list) =
  match l with
  | []        -> ()
  | (p,e)::[] -> (* last time *)
    if (first=false) then fprintf ppf "| " ;
    pp_open_hvbox ppf indent ;
    let (e,w) =
     (match e with
      | {pexp_desc = Pexp_when (e1, e2)} -> (e2, Some (e1))
      | _ -> (e, None)) in 
    pattern_with_when ppf w p ;
    fprintf ppf " ->@ " ;
    pp_open_hvbox ppf 0 ;
    expression_sequence ppf ~indent:0 e ;
    pp_close_box ppf () ;
    pp_close_box ppf () ;
  | (p,e)::r  -> (* not last  *)
    pp_open_hvbox ppf (indent + 2) ;
    if ((first=true) & (special_first_case=false)) then begin
      pp_print_if_newline ppf () ;
      pp_print_string ppf "  "
    end else
      fprintf ppf "| " ;
    let (e,w) =
     (match e with
      | {pexp_desc = Pexp_when (e1, e2)} -> (e2, Some (e1))
      | _ -> (e, None)) in 
    pattern_with_when ppf w p ;
    fprintf ppf " ->@ " ;
    pp_open_hvbox ppf 0 ;
    expression_sequence ppf ~indent:0 e ;
    pp_close_box ppf () ;
    pp_close_box ppf () ;
    pp_print_break ppf 1 0;
    (pattern_x_expression_case_list ppf ~first:false r);

and pattern_x_expression_def ppf (pat, e) =
  pattern pat ;
  fprintf ppf " =@ " ;
  expression e;

and pattern_list_helper ppf pat =
  match pat with
  | {ppat_desc = Ppat_construct ({txt=Longident.Lident("::")},
                   Some ({ppat_desc = Ppat_tuple([pat1; pat2])}),
                   _)}
    -> pattern pat1 ;
       fprintf ppf "@ ::@ " ;
       pattern_list_helper ppf pat2 ;
  | _ -> pattern pat ;

(* this prints the rest of a finite-list pattern, starting with a ';' *)
and pattern_finlist_helper ppf pat =
  match pat with
  | {ppat_desc = Ppat_construct ({txt=Longident.Lident("::")},
                   Some ({ppat_desc = Ppat_tuple([pat1; pat2])}),
                   _)}
    -> fprintf ppf "@ ;@ " ;
       pattern pat1 ;
       pattern_finlist_helper ppf pat2 ;
  | {ppat_desc = Ppat_construct ({txt=Longident.Lident("[]")}, _,_)} -> ()
  | _ -> assert false

and string_x_expression ({txt=s}, e) =
  pp_open_hovbox fmt indent ;
  fprintf fmt "%s =@ " s ;
  expression e ;
  pp_close_box fmt () ;

and longident_x_expression ({txt=li}, e) =
  pp_open_hovbox fmt indent ;
  fprintf fmt "%s =@ " (fmt_longident li);
  expression e;
  pp_close_box fmt () ;

and label_x_expression_param (l,e) =
  match l with 
  | ""  -> expression_param e ;
  | lbl ->
      if ((String.get lbl 0) = '?') then begin
        fprintf fmt "%s:" lbl ;
        expression_param e ;
      end else begin
        fprintf fmt "~%s:" lbl ;
        expression_param e ;
      end ;

and expression_param e =
  match e.pexp_desc with
    Pexp_ifthenelse _ -> expression_in_parens e ;
  | Pexp_function   _ -> expression_in_parens e ;
  | _ -> expression e ;

and expression_in_parens e =
  let already_has_parens =
    (match e.pexp_desc with
       Pexp_apply ({pexp_desc=Pexp_ident ({txt=Longident.Ldot (
         Longident.Lident(modname), funname)})},_)
         -> (match modname,funname with
             | "Array","get" -> false;
             | "Array","set" -> false;
             | _,_ -> true) ;
     | Pexp_apply ({pexp_desc=Pexp_ident ({txt=Longident.Lident(funname)})},_)
       -> (match funname with
           | "!" -> false;
           | _ -> true);
     | Pexp_apply (_,_) -> true;
     | Pexp_match (_,_) -> true;
     | Pexp_tuple (_) -> true ;
     | Pexp_constraint (_,_,_) -> true ;
     | _ -> false) in
  if (already_has_parens) then expression e
  else begin
    fprintf fmt "(" ;
    expression e ;
    fprintf fmt ")" ;
  end ;

and pattern_in_parens pat =
  let already_has_parens =
    match pat.ppat_desc with
    | Ppat_alias (_,_) -> true
    | Ppat_tuple (_) -> true
    | Ppat_or (_,_) -> true
    | Ppat_constraint (_,_) -> true
    | _ -> false in
  if (already_has_parens) then pattern pat
  else begin
    fprintf fmt "(" ;
    pattern pat ;
    fprintf fmt ")" ;
  end; 

and pattern_constr_params_option ppf po =
  match po with
  | None -> ();
  | Some pat ->
      pp_print_space ppf ();
      pattern_in_parens pat;

and type_variant_helper x =
  match x with
  | Rtag (l, b, ctl) ->  (* is b important? *)
      pp_open_hovbox fmt indent ; 
      fprintf fmt "`%s" l ; 
      if ((List.length ctl) != 0) then begin
        fprintf fmt " of@ " ;
        list2 core_type " *" ctl
      end ;
      pp_close_box fmt () ;
  | Rinherit (ct) ->
      core_type ct

(* prints a list of definitions as found in a let statement
   note! breaks "open and close boxes in same function" convention, however
         does always open and close the same number of boxes. (i.e. no "net
         gain or loss" of box depth.                                         *)
and pattern_x_expression_def_list ppf l =
  match l with
  | [] -> ()
  | hd :: tl ->
          pp_close_box ppf () ;
          pp_print_space ppf () ;
          pp_open_hvbox ppf indent ;
          fprintf ppf "and " ;
          pattern_x_expression_def ppf hd;
          pattern_x_expression_def_list ppf tl ;

(* end an if statement by printing an else phrase if there is an "else"
   statement in the ast. otherwise just close the box. *)
(* added: special case for "else if" case *)

and expression_eo ppf eo extra =
  match eo with
  | None   -> ();
  | Some x ->
    if extra then fprintf ppf " "
    else fprintf ppf "@ " ;
    match x.pexp_desc with
    | Pexp_ifthenelse (e1, e2, eo) ->   (* ... else if ...*)
        fprintf ppf "else" ;
        expression_elseif ppf (e1, e2, eo)
    | Pexp_sequence (e1, e2) ->
        fprintf ppf "else" ;
        expression_ifbegin ppf x;       (* ... else begin ... end*)
    | _ ->                              (* ... else ... *)
      pp_open_hvbox ppf indent ;
      fprintf ppf "else@ " ;
      expression x ;
      pp_close_box ppf () ;

and expression_elseif ppf (e1,e2,eo) =
      fprintf ppf " " ;
      expression_if_common ppf e1 e2 eo ;      

and expression_ifbegin ppf e =
      fprintf ppf " begin";
      pp_print_break ppf 1 indent ; (* "@;<1 2>"; *)
      expression_sequence ppf e;
      pp_print_break ppf 1 0 ; (* fprintf ppf "@;<1 0>" *)
      fprintf ppf "end";

and expression_if_common ppf e1 e2 eo =
  match e2.pexp_desc with
     | Pexp_sequence (e3, e4) ->
         fprintf ppf "if " ;
         expression e1;
         fprintf ppf " then" ;
         expression_ifbegin ppf e2;
         expression_eo ppf eo true;   (* ... then begin ... end *)
     | _ -> 
         pp_open_hvbox ppf indent ;
         fprintf ppf "if " ;
         expression e1;
         fprintf ppf " then@ " ;
         expression e2;
         pp_close_box ppf () ;
         expression_eo ppf eo false;

and expression_sequence ppf ?(skip=1) ?(indent=indent) ?(first=true) expr =
  if (first = true) then begin
    pp_open_hvbox ppf 0 ;
    expression_sequence ppf ~skip:skip ~indent:0 ~first:false expr ;
    pp_close_box ppf () ;
  end else
    match expr.pexp_desc with
    | Pexp_sequence (e1, e2) ->
         expression e1 ;
         fprintf ppf ";" ; 
         pp_print_break ppf skip indent ; (* "@;<1 2>" ; *)
         expression_sequence ppf ~skip:skip ~indent:indent ~first:false e2 ;
    | _ ->
         expression expr ;

and expression_list_helper ppf exp =
  match exp with
  | {pexp_desc = Pexp_construct ({txt=Longident.Lident("[]")}, None, _)}
     -> () ;
  | {pexp_desc = Pexp_construct ({txt=Longident.Lident("::")},
                   Some({pexp_desc = Pexp_tuple([exp1 ; exp2])}), _)}
     -> fprintf ppf ";@ " ;
        expression exp1 ;
        expression_list_helper ppf exp2 ; 
  | {pexp_desc = _}
     -> assert false;

and expression_list_nonterminal ppf exp =
  match exp with
  | {pexp_desc = Pexp_construct ({txt=Longident.Lident("[]")}, None, _)}
     -> fprintf ppf "[]" ; (* assert false; *)
  | {pexp_desc = Pexp_construct ({txt=Longident.Lident("::")},
                   Some({pexp_desc = Pexp_tuple([exp1 ; exp2])}), _)}
     -> expression exp1;
        fprintf ppf " ::@ ";
        expression_list_nonterminal ppf exp2;
  | {pexp_desc = _}
     -> expression exp;
;

and directive_argument = function
  | Pdir_none -> ()
  | Pdir_string (s) -> fprintf fmt "@ \"%s\"" s
  | Pdir_int (i) -> fprintf fmt "@ %d" i
  | Pdir_ident (li) -> fprintf fmt "@ %s" (fmt_longident li)
  | Pdir_bool (b) -> fprintf fmt "@ %s" (string_of_bool b)

and string_x_core_type_list (s, l) =
  p s;
  List.iter core_type l

and pattern_x_expression_case_single ppf (pat, e) eo lbl =
  (match eo with
     None ->   pattern_with_label ppf pat lbl
   | Some x -> fprintf ppf "?" ;
               pp_open_hovbox ppf indent ;
               fprintf ppf "(" ;
               pattern pat ;
               fprintf ppf " =@ " ;
               expression x ;
               fprintf ppf ")" ;
               pp_close_box ppf ()
  ) ;
  fprintf ppf " ->@ " ;
  expression_sequence ppf ~indent:0 e ;;

let rec toplevel_phrase = function
  | Ptop_def (s) ->
      pp_open_hvbox fmt 0;
      list2 structure_item ~breakfirst:false ~indent:0 "" s;
      pp_close_box fmt ();
  | Ptop_dir (s, da) ->
      pp_open_hovbox fmt indent;
      fprintf fmt "#%s" s;
      directive_argument da;
      pp_close_box fmt ()

end

*)

(* print code as a parse tree. Useful for debugging *)
let print_code_as_ast cde =
  let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
  Printast.implementation Format.std_formatter
  [{ pstr_desc = Pstr_eval (cde);
     pstr_loc  = Location.none }]

let format_code : Format.formatter -> 'a closed_code -> unit = fun ppf cde ->
  let cde = (cde : Trx.closed_code_repr :> Parsetree.expression) in
  Pprintast.expression ppf cde

(*
  let module M = PR(struct let ppf = ppf end) in
  fprintf ppf ".<@,"; M.expression x; fprintf ppf ">.@ ";
  try ignore (Trx.check_scope_extrusion x)
  with e -> fprintf ppf "\n%s" (Printexc.to_string e)
*)


(*
let top_phrase_pretty ppf x =
  let module M = PR(struct let ppf = ppf end) in
  pp_print_newline ppf () ;
  M.toplevel_phrase x;
  fprintf ppf ";;" ;
  pp_print_newline ppf ()
*)

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
  let (cde, check) = close_code_delay_check cde in
  print_closed_code ppf cde;
  try check ()
  with e -> fprintf ppf "\n%s" (Printexc.to_string e)
