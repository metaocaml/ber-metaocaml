# Build the BER MetaOCaml library, top-level, and the compiler driver
# See INSTALL for more detail

# Location of the OCaml distribution directory
ROOTDIR = ..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

COMPILER=$(ROOTDIR)/ocamlc
INCLUDES=-I $(ROOTDIR)/utils \
	-I $(ROOTDIR)/parsing  -I $(ROOTDIR)/typing -I $(ROOTDIR)/bytecomp \
	-I $(ROOTDIR)/lambda \
	-I $(ROOTDIR)/asmcomp  -I $(ROOTDIR)/driver \
	-I $(ROOTDIR)/toplevel -I $(ROOTDIR)/stdlib
COMPILER=$(ROOTDIR)/ocamlc
CAMLC=$(CAMLRUN) $(COMPILER) $(INCLUDES)
COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-41-42-44-45-48-70 \
          -warn-error A -bin-annot -nostdlib \
          -safe-string -strict-formats
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS += -O3
endif
OPTCOMPILER=$(ROOTDIR)/ocamlopt
CAMLOPT=$(CAMLRUN) $(OPTCOMPILER) -nostdlib -I $(ROOTDIR)/stdlib \
	-I $(ROOTDIR)/otherlibs/dynlink \
	-I $(ROOTDIR)/typing -I $(ROOTDIR)/parsing -I $(ROOTDIR)/utils
CAMLDEP=$(CAMLRUN) $(ROOTDIR)/tools/ocamldep

OC_CPPFLAGS += -I$(ROOTDIR)/runtime

METALIB=metalib.cma
METALIBOBJS=codelib.cmo lifts.cmo runcode.cmo
METAOBJS=$(METALIB) bertop.cmo berstart.cmo
OFFSHLIB=offshoring.cma
OFFSHOBJS=c_pp.cmo offshoringIR.cmo offshoringIR_pp.cmo offshoring.cmo


all: $(METAOBJS) metaocaml metaocamlc opt

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

offshoringIR_pp.cmo: offshoringIR_pp.ml c_ast.cmi
	$(CAMLC) $(COMPFLAGS) -no-alias-deps -c offshoringIR_pp.ml

$(METALIB): $(METALIBOBJS)
	$(CAMLC) $(COMPFLAGS) -o $@ -a $(METALIBOBJS)

$(OFFSHLIB): $(OFFSHOBJS)
	$(CAMLC) $(COMPFLAGS) -o $@ -a $(OFFSHOBJS)


install: metaocaml metaocamlmktop metaocamlc $(OFFSHLIB)
	cp runcode.cmi codelib.cmi lifts.cmi \
	  offshoringIR.cmi offshoringIR_pp.cmi offshoring.cmi \
	  c_pp.cmi c_ast.cmi \
	    $(METALIB) $(OFFSHLIB) bertop.cmo berstart.cmo \
	   $(LIBDIR)
	cp metaocaml $(BINDIR)/metaocaml$(EXE)
	cp metaocamlmktop $(BINDIR)/metaocamlmktop$(EXE)
	cp metaocamlc $(BINDIR)/metaocamlc$(EXE)

install.opt: opt
	cp runnative.cmi runnative.cmx runnative.o codelib.cmx codelib.o \
	   lifts.cmx lifts.o \
	  $(LIBDIR)
	cp metaocamlopt $(BINDIR)/metaocamlopt$(EXE)

metaocaml: $(METAOBJS) $(ROOTDIR)/compilerlibs/ocamlcommon.cma
# $(ROOTDIR)/tools/ocamlmktop \
# -o $@ $(METALIB) bertop.cmo berstart.cmo
	$(CAMLC) -nostdlib -linkall \
	$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
	$(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
	$(ROOTDIR)/compilerlibs/ocamltoplevel.cma \
	-o $@ $(METALIB) bertop.cmo  \
	$(ROOTDIR)/toplevel/topstart.cmo

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
#	cd gprint && $(MAKE) ROOTDIR=../$(ROOTDIR) clean

test: test-simple test-gadt test-pattern test-values test-genlet \
	test-lift test-offshore

test-simple:
	TERM=dumb $(BINDIR)/metaocaml < test/trivial.ml > trivial.out && \
	diff -u trivial.out trivial.ref
	TERM=dumb $(BINDIR)/metaocaml < test/simple.ml > simple.out && \
	diff -u simple.out simple.ref
	$(BINDIR)/metaocaml -w -8 test/simple_true.ml
	$(BINDIR)/metaocaml -w -8 test/quick_test.ml
	$(BINDIR)/metaocaml -w -8 test/test21.ml
	$(BINDIR)/metaocaml -w -8 test/test_levels.ml
#	$(BINDIR)/metaocaml test/t4.ml

test-gadt:
	cd test && \
	$(BINDIR)/ocamlc -c tgadt_decl.mli && \
	if $(BINDIR)/metaocamlc -c tgadt.ml; then \
          ( echo "Should have failed" && false ); else true; fi

test-pattern:
	TERM=dumb $(BINDIR)/metaocaml < test/pattern.ml > pattern.out && \
	diff -u pattern.out pattern.ref

test-values:
	TERM=dumb $(BINDIR)/metaocaml < test/values.ml > values.out && \
	diff -u values.out values.ref

test-genlet:
	TERM=dumb $(BINDIR)/metaocaml < test/genlet.ml > genlet.out && \
	diff -u genlet.out genlet.ref
	TERM=dumb $(BINDIR)/metaocaml test/fib.ml
	TERM=dumb $(BINDIR)/metaocaml < test/genletrec.ml > genletrec.out && \
	diff -u genletrec.out genletrec.ref

test-lift:
	$(BINDIR)/metaocaml test/test_lift.ml

.PHONY: test-offshore test-offshore1 test-offshore2
test-offshore: test-offshore2

test-offshore1:
	$(BINDIR)/metaocamlc -c test/mockgpu.ml && \
	TERM=dumb $(BINDIR)/metaocaml -w -8 offshoring.cma -I test/ \
	< test/offshore_simple.ml > offshore_simple.out && \
	diff -u offshore_simple.out offshore_simple.ref

test-offshore2:
	TERM=dumb $(BINDIR)/metaocaml -w -8 -I test/ \
	offshoring.cma test/offshore.ml

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
	$(BINDIR)/metaocamlc -o off offshoring.cma test/offshore.ml
	./off
	rm -f off
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
	cp -p patches/trx.ml $(ROOTDIR)/typing/
	cp -p patches/trx.mli $(ROOTDIR)/typing/
#	patch -p0 -d $(ROOTDIR) < patches/patch
	patch -p1 -d $(ROOTDIR) < patches/patch

# Create the patch set
makepatch:
	cp -p $(ROOTDIR)/typing/trx.ml patches/
	cp -p $(ROOTDIR)/typing/trx.mli patches/
	sh ./build_patch.sh > patches/patch

# metaocamlopt: stdlib/metanative.cmxa tools/metaocamlopt.tpl 
# 	sed -e 's|%%BINDIR%%|$(BINDIR)|' tools/metaocamlopt.tpl > metaocamlopt
# 	chmod +x metaocamlopt

include .depend

depend:
	$(CAMLDEP) *.mli *.ml > .depend

.PHONY: all test clean patch makepatch depend test-compile install
