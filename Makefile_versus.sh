#!/bin/bash
ocamlc types.mli
ocamlc fonctions.mli
ocamlc fonctions_jeu.mli
ocamlc graph.mli
ocamlc heur.mli
ocamlc alphabeta.mli
ocamlc versus.mli

ocamlc -o main_versus.out unix.cma graphics.cma types.ml fonctions.ml fonctions_jeu.ml graph.ml heur.ml alphabeta.ml versus.ml main_versus.ml

rm -f *.cm[io]
