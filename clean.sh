#!/bin/sh
cd sim
ocamlbuild -clean
cd ../assembleur
ocamlbuild -clean
cd ..
rm *.net *.byte
