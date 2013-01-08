# Building and testing the Generic Print facility
#
# 	make all
#	     to build the library
#	make test_toponly
#	     test with the top-level. No need to compile anything
#	make testb
#	     test gprint in byte-code programs
#	make testo
#	     test gprint in natively-compiled programs
#
#
# $Id: Makefile,v 1.2 2006/04/17 01:46:50 oleg Exp $

# Location of the OCaml distribution directory
CAMLROOT=../..

include $(CAMLROOT)/config/Makefile

RUNTIME=$(CAMLROOT)/boot/ocamlrun
INCLUDES=-I $(CAMLROOT)/utils \
	-I $(CAMLROOT)/parsing  -I $(CAMLROOT)/typing -I $(CAMLROOT)/bytecomp \
	-I $(CAMLROOT)/asmcomp  -I $(CAMLROOT)/driver \
	-I $(CAMLROOT)/toplevel -I $(CAMLROOT)/boot \
	-I $(CAMLROOT)/metalib
COMPILER=$(CAMLROOT)/ocamlc
CAMLC=$(RUNTIME) $(COMPILER) $(INCLUDES)
COMPFLAGS=-g -warn-error A -nostdlib
OPTCOMPILER=$(CAMLROOT)/ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-warn-error A -nostdlib
CAMLDEP=$(CAMLROOT)/boot/ocamlrun $(CAMLROOT)/tools/ocamldep
MOCAMLTOP=$(BINDIR)/metaocaml
MOCAMLC=$(BINDIR)/metaocamlc


.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) -c $<


all: gprint.cma

install:
	cp gprint.cmi gprint.cma  \
	  $(LIBDIR)


# MetaOCaml modules to build the library.
# The order is mighty important! In particular, ident.cmo must be first!
LIBLIBMOD= ident.cmo misc.cmo path.cmo types.cmo btype.cmo tbl.cmo \
	subst.cmo predef.cmo datarepr.cmo config.cmo consistbl.cmo \
	env.cmo clflags.cmo ctype.cmo printast.cmo oprint.cmo \
	primitive.cmo printtyp.cmo genprintval.cmo
LIBINCLUDES=-I $(OCAMLROOT)/parsing \
	-I $(OCAMLROOT)/typing      \
	-I $(OCAMLROOT)/toplevel    \
	-I $(OCAMLROOT)/utils

# METANATIVE=ident.cmx utils/misc.cmx utils/tbl.cmx utils/config.cmx \
# 	utils/consistbl.cmx \
#   utils/clflags.cmx utils/terminfo.cmx utils/ccomp.cmx utils/warnings.cmx \
#   parsing/linenum.cmx parsing/location.cmx parsing/longident.cmx \
#   parsing/syntaxerr.cmx parsing/parser.cmx \
#   parsing/lexer.cmx  parsing/parse.cmx parsing/printast.cmx \
# typing/path.cmx \
#   typing/primitive.cmx typing/types.cmx \
#   typing/btype.cmx \
#   typing/subst.cmx typing/predef.cmx \
#   typing/datarepr.cmx typing/env.cmx \
#   typing/typedtree.cmx typing/ctype.cmx \
#   typing/oprint.cmx typing/printtyp.cmx typing/includeclass.cmx \
#   typing/mtype.cmx typing/includecore.cmx \
#   typing/includemod.cmx typing/parmatch.cmx \
#   typing/typetexp.cmx typing/stypes.cmx typing/typecore.cmx \
#   typing/typedecl.cmx typing/typeclass.cmx \
#   typing/typemod.cmx \
# bytecomp/lambda.cmx bytecomp/printlambda.cmx \
#   bytecomp/typeopt.cmx bytecomp/switch.cmx bytecomp/matching.cmx \
#   bytecomp/cprint.cmx bytecomp/f90abs.cmx bytecomp/cabs.cmx bytecomp/dl.cmx \
#   bytecomp/trxtime.cmx bytecomp/trx.cmx \
#   bytecomp/translobj.cmx bytecomp/translcore.cmx \
#   bytecomp/translclass.cmx bytecomp/translmod.cmx \
#   bytecomp/simplif.cmx bytecomp/runtimedef.cmx \
# asmcomp/arch.cmx asmcomp/cmm.cmx asmcomp/printcmm.cmx \
#   asmcomp/reg.cmx asmcomp/mach.cmx asmcomp/proc.cmx \
#   asmcomp/clambda.cmx asmcomp/compilenv.cmx \
#   asmcomp/closure.cmx asmcomp/cmmgen.cmx \
#   asmcomp/printmach.cmx asmcomp/selectgen.cmx asmcomp/selection.cmx \
#   asmcomp/comballoc.cmx asmcomp/liveness.cmx \
#   asmcomp/spill.cmx asmcomp/split.cmx \
#   asmcomp/interf.cmx asmcomp/coloring.cmx \
#   asmcomp/reloadgen.cmx asmcomp/reload.cmx \
#   asmcomp/printlinear.cmx asmcomp/linearize.cmx \
#   asmcomp/schedgen.cmx asmcomp/scheduling.cmx \
#   asmcomp/emitaux.cmx asmcomp/emit.cmx asmcomp/asmgen.cmx \
#   asmcomp/asmlink.cmx asmcomp/asmlibrarian.cmx asmcomp/asmpackager.cmx \
#   unused_var.cmx \
# driver/pparse.cmx driver/opterrors.cmx driver/optcompile.cmx \
# natdyn/natdynlink.cmx\
# stdlib/hookdynlink.cmx


gprint.cma: gprint.ml gprint.cmi
	$(OCAMLC) $(LIBINCLUDES) -o $@ -a $(LIBLIBMOD) gprint.ml

# gprint.cmxa: gprint.ml gprint.cmi
# 	$(MOCAMLOPTC) $(LIBINCLUDES) -I $(OCAMLROOT) -o $@ -a $(METANATIVE) \
# 	$(OCAMLROOT)/toplevel/genprintval.ml \
# 	gprint.ml

# .PHONY: test_toponly
# test_toponly:
# 	(sed "s?/home/oleg/Cache/ometa-cvs?$(OCAMLROOT)?g" gprint_toplevel.ml;\
# 	sed "s?open Gprint?open Format?g" vgprint.ml) | \
# 	$(OCAMLTOP)

# testb:	gprint.cma gprint.cmi vgprint.ml
# 	$(OCAMLC) -o $@ gprint.cma vgprint.ml
# 	./$@

vgprint_top: vgprint_aux.cmi
	$(MOCAMLTOP) vgprint.ml

vgprint: vgprint_aux.cmi
	$(MOCAMLC) -o $@ vgprint.ml
	./vgprint

clean::
	rm -f testb vgprint

# testo:	gprint.cmxa gprint.cmi vgprint.ml
# 	$(MOCAMLOPTC) -o $@ gprint.cmxa   \
# 	hookdynlink.cmx -ccopt -Wl,-E -cclib -lnatdyn vgprint.ml
# 	./$@

clean::
	rm -f testo

clean::
	rm -f *.cm[io] *.[oa] *~
