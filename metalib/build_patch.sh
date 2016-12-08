#!/bin/sh
# Building the patch set
#    build_patch
# This auxiliary file builds the patch set using the internal
# GIT repository. The patch set is written into the standard output.

cd ..

#git diff 327f91b41f16b223c783070fa44058c5b1db8804 -- 
git diff 4.02 -- \
./.depend \
./Makefile \
./parsing/lexer.mll \
./parsing/parser.mly \
./parsing/pprintast.ml \
./typing/predef.ml \
./typing/predef.mli \
./typing/typecore.ml

# svn diff -r5522 \
# tools/addlabels.ml \
