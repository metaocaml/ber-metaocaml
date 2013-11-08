# Make the distribution directory

DEST=/tmp/ber-metaocaml-100

mkdir $DEST

cp -p \
.depend \
ChangeLog \
Files.txt \
INSTALL \
Makefile \
NOTES.txt \
ORIGINAL-LICENSE-META \
Problems.txt \
README \
berstart.ml \
bertop.ml \
build_patch.sh \
metaocamlmktop.tpl \
metaocamlc.tpl \
print_code.ml \
print_code.mli \
runcode.ml \
runcode.mli \
simple.ref \
trivial.ref \
$DEST/

mkdir $DEST/patches
cp -p patches/* $DEST/patches

mkdir $DEST/test
cp -p test/*.ml test/*.mli $DEST/test

# mkdir $DEST/gprint
# cp -p gprint/Makefile $DEST/gprint
# cp -p gprint/*.ml  $DEST/gprint
# cp -p gprint/*.mli $DEST/gprint
