#!/bin/sh
cd sim
sh COMPIL.sh
cp netlist_sim.native ..
cp scheduler_test.native ..
cd ../assembleur
make
cd ..
./mjc.native proco.mj
./assembleur/arm-as clock.s
./scheduler_test.native proco.net < clock.byte
