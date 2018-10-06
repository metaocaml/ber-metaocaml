# Build the BER MetaOCaml library, top-level, and the compiler driver
# See INSTALL for more detail

# Location of the OCaml distribution directory
CAMLROOT=..

include $(CAMLROOT)/config/Makefile

RUNTIME=$(CAMLROOT)/boot/ocamlrun
INCLUDES=-I $(CAMLROOT)/utils \
	-I $(CAMLROOT)/parsing  -I $(CAMLROOT)/typing -I $(CAMLROOT)/bytecomp \
	-I $(CAMLROOT)/asmcomp  -I $(CAMLROOT)/driver \
	-I $(CAMLROOT)/toplevel -I $(CAMLROOT)/stdlib
COMPILER=$(CAMLROOT)/ocamlc
CAMLC=$(RUNTIME) $(COMPILER) $(INCLUDES)
COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-41-42-44-45-48 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats
OPTCOMPILER=$(CAMLROOT)/ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER) -nostdlib -I $(CAMLROOT)/stdlib \
	-I $(CAMLROOT)/otherlibs/dynlink \
	-I $(CAMLROOT)/typing -I $(CAMLROOT)/parsing -I $(CAMLROOT)/utils
CAMLDEP=$(CAMLROOT)/boot/ocamlrun $(CAMLROOT)/tools/ocamldep

METALIB=metalib.cma
METALIBOBJS=codelib.cmo lifts.cmo runcode.cmo offshoringIR.cmo
METAOBJS=$(METALIB) bertop.cmo berstart.cmo

all: $(METAOBJS) metaocaml metaocamlc opt

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

$(METALIB): $(METALIBOBJS)
	$(CAMLC) $(COMPFLAGS) -o $@ -a $(METALIBOBJS)


install: metaocaml metaocamlmktop metaocamlc
	cp runcode.cmi codelib.cmi lifts.cmi offshoringIR.cmi \
	   metalib.cma bertop.cmo berstart.cmo \
	  $(LIBDIR)
	cp metaocaml $(BINDIR)/metaocaml$(EXE)
	cp metaocamlmktop $(BINDIR)/metaocamlmktop$(EXE)
	cp metaocamlc $(BINDIR)/metaocamlc$(EXE)

install.opt: opt
	cp runnative.cmi runnative.cmx runnative.o codelib.cmx codelib.o \
	   lifts.cmx lifts.o \
	  $(LIBDIR)
	cp metaocamlopt $(BINDIR)/metaocamlopt$(EXE)

metaocaml: $(METAOBJS) $(CAMLROOT)/compilerlibs/ocamlcommon.cma
# $(CAMLROOT)/tools/ocamlmktop \
# -o $@ $(METALIB) bertop.cmo berstart.cmo
	$(CAMLC) -nostdlib -linkall \
	$(CAMLROOT)/compilerlibs/ocamlcommon.cma \
	$(CAMLROOT)/compilerlibs/ocamlbytecomp.cma \
	$(CAMLROOT)/compilerlibs/ocamltoplevel.cma \
	-o $@ $(METALIB) bertop.cmo  \
	$(CAMLROOT)/toplevel/topstart.cmo

clean::
	rm -f metaocaml

metaocamlmktop: metaocamlmktop.tpl metaocaml
	sed -e 's|%%BINDIR%%|$(BINDIR)|' metaocamlmktop.tpl > metaocamlmktop
	chmod +x metaocamlmktop

clean::
	rm -f metaocamlmktop

metaocamlc: metaocamlc.c
	$(CC) -Wall -DBINDIR=\"$(BINDIR)\" -o metaocamlc metaocamlc.c

clean::
	rm -f metaocamlc

opt: metaocamlopt codelib.cmx runnative.cmx lifts.cmx

metaocamlopt: metaocamlc.c 
	$(CC) -Wall -DBINDIR=\"$(BINDIR)\" -DOPT -o metaocamlopt metaocamlc.c

clean::
	rm -f metaocamlopt

clean::
	rm -f *.cm* *.o *.a
	rm -f test/*.cm* test/*.o
	rm -f *~
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) clean

test: test-simple test-gadt test-pattern test-values test-genlet \
	test-lift

test-simple:
	TERM=dumb $(BINDIR)/metaocaml -w -8 < test/trivial.ml > trivial.out && \
	diff -u trivial.out trivial.ref
	TERM=dumb $(BINDIR)/metaocaml -w -8 < test/simple.ml > simple.out && \
	diff -u simple.out simple.ref
	$(BINDIR)/metaocaml -w -8 test/simple_true.ml
	$(BINDIR)/metaocaml -w -8 test/quick_test.ml
	$(BINDIR)/metaocaml -w -8 test/test21.ml
	$(BINDIR)/metaocaml -w -8 test/test_levels.ml
#	$(BINDIR)/metaocaml test/t4.ml
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) MOCAMLTOP="../metaocaml -I .." testv_top
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) vgprint_top

test-gadt:
	cd test && \
	$(BINDIR)/ocamlc -c tgadt_decl.mli && \
	if $(BINDIR)/metaocamlc -c tgadt.ml; then \
          ( echo "Should have failed" && false ); else true; fi

test-pattern:
	TERM=dumb $(BINDIR)/metaocaml -w -8 < test/pattern.ml > pattern.out && \
	diff -u pattern.out pattern.ref

test-values:
	TERM=dumb $(BINDIR)/metaocaml -w -8 < test/values.ml > values.out && \
	diff -u values.out values.ref

test-genlet:
	TERM=dumb $(BINDIR)/metaocaml -w -8 < test/genlet.ml > genlet.out && \
	diff -u genlet.out genlet.ref
	TERM=dumb $(BINDIR)/metaocaml -w -8 test/fib.ml

test-lift:
	$(BINDIR)/metaocaml -w -8 test/test_lift.ml

test-offshore:
	$(BINDIR)/metaocamlc -o off test/offshore_simple.ml
	./off
	rm -f off

test-compile:
	cd test && \
	$(BINDIR)/metaocamlc -w -8 -o csp1 \
	test_csp1.mli test_csp1.ml test_csp1r.ml && \
	./csp1 && rm -f csp1
	$(BINDIR)/metaocamlc -w -8 -o simple_true test/simple_true.ml
	./simple_true
	rm -f simple_true
# 	$(BINDIR)/metaocamlc -o quick_test test/quick_test.ml
# 	./quick_test
	$(BINDIR)/metaocamlc -dparsetree -drawlambda -dinstr -c test/test21.ml
	$(BINDIR)/metaocamlc -c test/t4types.mli && \
	mv test/t4types.cmi . # .cmi file should be where the executable is
	$(BINDIR)/metaocamlc -w -8 -o t4 -I test/ test/t4.ml
	./t4
	rm -f t4 t4types.cmi
	$(BINDIR)/metaocamlc -w -8 -o fib -I test/ test/fib.ml
	./fib
	rm -f fib
	$(BINDIR)/metaocamlc -w -8 -o tlift test/test_lift.ml
	./tlift
	rm -f tlift
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) vgprint
# Testing setting of the .cmo path
	$(BINDIR)/metaocamlc test/test_path_a.ml
	mv test/test_path_a.cm[io] /tmp
	$(BINDIR)/metaocamlc -I /tmp -o tp test/test_path.ml
	./tp
	rm -f tp

test-native:
	cd test && \
	$(BINDIR)/metaocamlopt -verbose -o qtn square.ml quick_test_native.ml && \
	./qtn && rm -f qtn

clean::
	rm -f simple_true quick_test t4 fib tlift \
	pt trivial.out simple.out a.out tp test/qtn

clean::
	rm -f test/csp1 test/*.cmi test/*.cmo

# Patch the OCaml distribution
patch:
	cp -p patches/trx.ml $(CAMLROOT)/typing/
	cp -p patches/trx.mli $(CAMLROOT)/typing/
#	patch -p0 -d $(CAMLROOT) < patches/patch
	patch -p1 -d $(CAMLROOT) < patches/patch

# Create the patch set
makepatch:
	cp -p $(CAMLROOT)/typing/trx.ml patches/
	cp -p $(CAMLROOT)/typing/trx.mli patches/
	sh ./build_patch.sh > patches/patch

# metaocamlopt: stdlib/metanative.cmxa tools/metaocamlopt.tpl 
# 	sed -e 's|%%BINDIR%%|$(BINDIR)|' tools/metaocamlopt.tpl > metaocamlopt
# 	chmod +x metaocamlopt

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend

.PHONY: all test clean patch makepatch depend test-compile install
