#!/bin/sh
# Building the patch set
#    build_patch
# This auxiliary file builds the patch set using the internal
# SVN repository. The patch set is written into the standard output.

cd ..

svn diff -r5522 \
tools/addlabels.ml \
tools/depend.ml \
tools/dumpobj.ml \
tools/ocamlprof.ml \
typing/unused_var.ml \
typing/ident.mli  \
typing/typemod.ml \
typing/env.ml \
typing/typecore.ml \
typing/typedtree.ml \
typing/predef.ml \
typing/typeclass.ml \
typing/typedtree.mli \
typing/predef.mli \
typing/env.mli \
typing/typecore.mli \
typing/ident.ml \
.depend \
Makefile \
bytecomp/lambda.ml \
bytecomp/symtable.ml \
bytecomp/printlambda.ml \
bytecomp/translcore.ml \
bytecomp/lambda.mli \
parsing/parser.mly \
parsing/lexer.mll \
parsing/parsetree.mli \
parsing/printast.ml \
ocamldoc/Makefile \

