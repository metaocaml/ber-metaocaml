#!/bin/sh
# Building the patch set
#    build_patch
# This auxiliary file builds the patch set using the internal
# SVN repository. The patch set is written into the standard output.

cd ..

git diff 4.00.1 -- \
./.depend \
./Makefile \
./ocamldoc/Makefile \
./parsing/lexer.mll \
./parsing/parser.mly \
./parsing/parsetree.mli \
./parsing/printast.ml \
./bytecomp/lambda.ml \
./bytecomp/lambda.mli \
./bytecomp/printlambda.ml \
./bytecomp/translcore.ml \
./bytecomp/symtable.ml \
./tools/depend.ml \
./tools/addlabels.ml \
./tools/dumpobj.ml \
./tools/ocamlprof.ml \
./typing/typedtree.mli \
./typing/typedtree.ml \
./typing/predef.ml \
./typing/predef.mli \
./typing/printtyped.ml \
./typing/cmt_format.ml \
./typing/env.mli \
./typing/env.ml \
./typing/typecore.mli \
./typing/typecore.ml \
./typing/typemod.ml  \
./tools/untypeast.ml \
./tools/typedtreeIter.ml

# svn diff -r5522 \
# tools/addlabels.ml \
