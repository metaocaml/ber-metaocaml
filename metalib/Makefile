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
COMPFLAGS=-strict-sequence -w +33..39 -warn-error A -nostdlib
OPTCOMPILER=$(CAMLROOT)/ocamlopt
CAMLOPT=$(RUNTIME) $(OPTCOMPILER)
OPTCOMPFLAGS=-strict-sequence -w +33..39 -warn-error A -nostdlib
CAMLDEP=$(CAMLROOT)/boot/ocamlrun $(CAMLROOT)/tools/ocamldep

METALIB=metalib.cma
METALIBOBJS=runcode.cmo print_code.cmo
METAOBJS=$(METALIB) bertop.cmo berstart.cmo

all: $(METAOBJS) metaocaml metaocamlc

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTCOMPFLAGS) -c $<

$(METALIB): $(METALIBOBJS)
	$(CAMLC) $(COMPFLAGS) -o $@ -a $(METALIBOBJS)


install: metaocaml metaocamlmktop metaocamlc
	cp runcode.cmi print_code.cmi metalib.cma bertop.cmo berstart.cmo \
	  $(LIBDIR)
	cp metaocaml $(BINDIR)/metaocaml$(EXE)
	cp metaocamlmktop $(BINDIR)/metaocamlmktop$(EXE)
	cp metaocamlc $(BINDIR)/metaocamlc$(EXE)

metaocaml: $(METAOBJS) $(CAMLROOT)/compilerlibs/ocamlcommon.cma
# $(CAMLROOT)/tools/ocamlmktop \
# -o $@ $(METALIB) bertop.cmo berstart.cmo
	$(CAMLC) -nostdlib -linkall \
	$(CAMLROOT)/compilerlibs/ocamlcommon.cma \
	$(CAMLROOT)/compilerlibs/ocamlbytecomp.cma \
	$(CAMLROOT)/compilerlibs/ocamltoplevel.cma \
	-o $@ $(METALIB) bertop.cmo berstart.cmo \
	$(CAMLROOT)/toplevel/topstart.cmo



clean::
	rm -f metaocaml

metaocamlmktop: metaocamlmktop.tpl metaocaml
	sed -e 's|%%BINDIR%%|$(BINDIR)|' metaocamlmktop.tpl > metaocamlmktop
	chmod +x metaocamlmktop

clean::
	rm -f metaocamlmktop

metaocamlc: metaocamlc.tpl metaocaml
	sed -e 's|%%BINDIR%%|$(BINDIR)|' metaocamlc.tpl > metaocamlc
	chmod +x metaocamlc

clean::
	rm -f metaocamlc

clean::
	rm -f *.cm* *.o *.a
	rm -f test/*.cm* test/*.o
	rm -f *~
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) clean

test:
	TERM=dumb $(BINDIR)/metaocaml < test/trivial.ml > trivial.out && \
	diff -u trivial.out trivial.ref
	TERM=dumb $(BINDIR)/metaocaml < test/simple.ml > simple.out && \
	diff -u simple.out simple.ref
	$(BINDIR)/metaocaml test/simple_true.ml
	$(BINDIR)/metaocaml test/quick_test.ml
	$(BINDIR)/metaocaml test/test21.ml
	$(BINDIR)/metaocaml test/pythagorian_triples.ml
	$(BINDIR)/metaocaml test/test_levels.ml
#	$(BINDIR)/metaocaml test/t4.ml
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) MOCAMLTOP="../metaocaml -I .." testv_top
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) vgprint_top

test-compile:
	$(BINDIR)/metaocamlc -o simple_true test/simple_true.ml
	./simple_true
# 	$(BINDIR)/metaocamlc -o quick_test test/quick_test.ml
# 	./quick_test
	$(BINDIR)/metaocamlc -dparsetree -drawlambda -dinstr -c test/test21.ml
	$(BINDIR)/metaocamlc -c test/t4types.mli && \
	cp test/t4types.cmi . # .cmi file should be where the executable is
	$(BINDIR)/metaocamlc -o t4 -I test/ test/t4.ml
	./t4
	$(BINDIR)/metaocamlc -o pt test/pythagorian_triples.ml
	./pt
#	cd gprint && $(MAKE) CAMLROOT=../$(CAMLROOT) vgprint
# Testing setting of the .cmo path
	$(BINDIR)/metaocamlc test/test_path_a.ml
	mv test/test_path_a.cm[io] /tmp
	$(BINDIR)/metaocamlc -o tp test/test_path.ml
	./tp

clean::
	rm -f simple_true quick_test t4 pt trivial.out simple.out tp

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
