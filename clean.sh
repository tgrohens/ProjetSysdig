!/bin/sh
cd sim
ocamlbuild -clean
cd ../assembleur
ocamlbuild -clean
cd ..
rm proco.net clock.byte
