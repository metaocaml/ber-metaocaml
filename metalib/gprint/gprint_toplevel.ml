(**			Generic print

This file is for using the generic print facility with metaocaml:
unmodified MetaOCaml toplevel.

First, correct the paths below (to point to the MetaOCaml installation)
and then #use the file.

The file loads a few modules needed by the generic print facility.

THE ORDER OF LOADING IS IMPORTANT!!!

In particular, "ident.cmo" must be loaded first. It is a stateful module,
whose state is the typestamp for identifiers. The module Predef uses
Ident.create when building identifiers for pre-defined types such as
int. It is imperative that predefined identifiers accessible via
Predef here have the same timestamp as those that existed in the host
code: so Meta-level and the host-level use the same predefined type
identifiers.

   $Id: gprint_toplevel.ml,v 1.2 2006/04/15 11:27:37 oleg Exp $
*)

#directory "/home/oleg/Cache/ometa-cvs/parsing";;
#directory "/home/oleg/Cache/ometa-cvs/typing";;
#directory "/home/oleg/Cache/ometa-cvs/toplevel";;
#directory "/home/oleg/Cache/ometa-cvs/utils";;


(* This must be loaded first! It is stateful, and affects Predef *)
#load "ident.cmo";; 

#load "misc.cmo";;
#load "path.cmo";;
#load "types.cmo";;
#load "btype.cmo";;
#load "tbl.cmo";;
#load "subst.cmo";;
#load "predef.cmo";;
#load "datarepr.cmo";;
#load "config.cmo";;
#load "consistbl.cmo";;
#load "env.cmo";;
#load "clflags.cmo";;
#load "ctype.cmo";;
#load "printast.cmo";;
#load "oprint.cmo";;
#load "primitive.cmo";;
#load "printtyp.cmo";;
#load "genprintval.cmo";;


(* Finally, load ou gprint proper *)
#use "gprint.ml";;
