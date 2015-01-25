#!/bin/sh
ocamlbuild -use-menhir -libs unix scheduler_test.native
ocamlbuild -use-menhir -libs unix netlist_sim.native
