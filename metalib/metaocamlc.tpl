#!/bin/sh

# Multi-shell script.  Works under Bourne Shell, MPW Shell, zsh.

if : == x
then # Bourne Shell or zsh
     exec %%BINDIR%%/ocamlc -I +compiler-libs ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma metalib.cma berstart.cmo "$@"
else # MPW Shell
     ocamlc -I +compiler-libs ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma metalib.cma berstart.cmo {"parameters"}
     exit {status}
End # uppercase E because "end" is a keyword in zsh
fi
