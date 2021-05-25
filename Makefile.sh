#!/bin/bash
ocamlc types.mli
ocamlc fonctions.mli
ocamlc fonctions_jeu.mli
ocamlc graph.mli
ocamlc heur.mli
ocamlc alphabeta.mli
ocamlc optimisation.mli

ocamlc -o main.out unix.cma graphics.cma types.ml fonctions.ml fonctions_jeu.ml graph.ml heur.ml alphabeta.ml optimisation.ml main.ml

rm -f *.cm[io]
