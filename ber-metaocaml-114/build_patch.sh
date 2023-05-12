#!/bin/sh
# Building the patch set
#    build_patch
# This auxiliary file builds the patch set using the internal
# GIT repository. The patch set is written into the standard output.

cd ..

#git diff 327f91b41f16b223c783070fa44058c5b1db8804 -- 
git diff --text 4.14.1 -- \
./.depend \
./Makefile \
./compilerlibs/Makefile.compilerlibs \
./parsing/lexer.mll \
./parsing/parser.mly \
./parsing/pprintast.ml \
./boot/menhir/parser.mli \
./boot/menhir/parser.ml \
./boot/menhir/menhirLib.mli \
./boot/menhir/menhirLib.ml \
./typing/typecore.ml

# svn diff -r5522 \
# tools/addlabels.ml \
