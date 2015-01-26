!/bin/sh
cd sim
sh COMPIL.sh
cd ../assembleur
make
cd ..
./mjc.native proco.mj
./assembleur/arm-as clock.s
./sim/netlist_sim.native proco.net < clock.byte
