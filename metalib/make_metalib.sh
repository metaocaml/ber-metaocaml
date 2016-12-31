# Make the distribution directory

DEST=/tmp/ber-metaocaml-104

mkdir $DEST

cp -p \
.depend \
ChangeLog \
Files.txt \
INSTALL \
Makefile \
NOTES.txt \
Problems.txt \
README \
berstart.ml \
bertop.ml \
build_patch.sh \
metaocamlmktop.tpl \
metaocamlc.c \
print_code.ml \
print_code.mli \
runcode.ml \
runcode.mli \
runnative.mli \
runnative.ml \
metaocamlopt.c \
simple.ref \
trivial.ref \
values.ref \
pattern.ref \
genlet.ref \
$DEST/

mkdir $DEST/patches
cp -p patches/* $DEST/patches

mkdir $DEST/test
cp -p test/*.ml test/*.mli $DEST/test

# mkdir $DEST/gprint
# cp -p gprint/Makefile $DEST/gprint
# cp -p gprint/*.ml  $DEST/gprint
# cp -p gprint/*.mli $DEST/gprint
