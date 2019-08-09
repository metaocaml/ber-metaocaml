#!/bin/sh
# Building the patch set
#    build_patch
# This auxiliary file builds the patch set using the internal
# GIT repository. The patch set is written into the standard output.

cd ..

#git diff 327f91b41f16b223c783070fa44058c5b1db8804 -- 
git diff 4.01 -- \
./.depend \
./Makefile \
./ocamldoc/Makefile \
./parsing/lexer.mll \
./parsing/parser.mly \
./parsing/parsetree.mli \
./parsing/printast.ml \
./parsing/pprintast.ml \
./parsing/ast_mapper.ml \
./typing/typedtree.mli \
./typing/typedtree.ml \
./typing/predef.ml \
./typing/predef.mli \
./typing/printtyped.ml \
./typing/typedtreeIter.ml \
./typing/typedtreeMap.ml \
./typing/env.mli \
./typing/env.ml \
./typing/envaux.ml \
./typing/typecore.mli \
./typing/typecore.ml \
./typing/typemod.ml  \
./bytecomp/lambda.ml \
./bytecomp/lambda.mli \
./bytecomp/printlambda.ml \
./bytecomp/translcore.ml \
./bytecomp/symtable.ml \
./tools/depend.ml \
./tools/dumpobj.ml \
./tools/ocamlprof.ml \
./tools/untypeast.ml \
./tools/tast_iter.ml \
./asmcomp/cmmgen.ml


# svn diff -r5522 \
# tools/addlabels.ml \
