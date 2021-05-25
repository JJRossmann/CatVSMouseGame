open Types
open Fonctions
open Fonctions_jeu
open Heur

let rec alphabetarec maxdepth depth game alpha beta minimise player = (*the alphabeta algorithm, recursively returns the score of a node*)
    (*the variable player is the current player in the game, who has to choose a move*)
    let winner = game.winner in 
    if !winner != -1 && !winner = player then float_of_int (maxdepth - depth + 1) (*tests if there is a winner, and if it's the player return a highly positive score depending on the current depth in the tree*)
    else if !winner != - 1 then float_of_int (- (maxdepth - depth + 1)) (*if someone won but it isn't the current player, return a highly negative score depending on the depth*)
    (*the 2 previous make it so that the game ends faster, the current player will go for nodes that make him win faster*)
    else if depth = maxdepth then heuristic_match game player (*if we arrive at the maximum depth we gave and nobody has won, return a score calculated with a certain heuristic*)
    else
    if minimise = true then
        let l = possible_games game in
        let m = ref beta in
        let i = ref 0 in
        let b = ref false in

        let rec recursive_search x bol =
        match (x,bol) with
                _,true -> !m
                |3,_ -> !m
                |x,bol -> begin
                          m:= min !m (alphabetarec maxdepth (depth + 1) l.(x) alpha !m false player);
                          if !m <= alpha then recursive_search (x+1) true
                          else recursive_search (x+1) bol
                          end
        in
        recursive_search !i !b;

    else
        let l = possible_games game in
        let m = ref alpha in
        let i = ref 0 in
        let b = ref false in

        let rec recursive_search x bol =
        match (x,bol) with
                _,true -> !m
                |3,_ -> !m
                |x,bol -> begin
                          m:= max  !m (alphabetarec maxdepth (depth + 1) l.(x) !m beta true player);
                          if !m >=beta  then recursive_search (x+1) true
                          else recursive_search (x+1) bol
                          end
        in
        recursive_search !i !b;;


let alphabeta game = (*function that calls the recursive alphabeta algorithm on the first 3 possible next game states, but returns the chosen game instead of the score*)
    let l = possible_games game in (*creates the list of the next possible game states*)
    let m = ref neg_infinity in
    let chosen_game = ref 0 in
    let player = !(game.current_player) in

    (*Creating the alphabeta tree *)
    let start_tree = 0 in
    let end_tree = 3 in
    let fun_end i = l.(!chosen_game) in (*returns the chosen game in the end *)
    let fun_rec i =
        let a = alphabetarec maxdepth 1 l.(i) !m infinity true player in
        if !m < a then begin m:= a; chosen_game:= i end
    in
    iter1 start_tree end_tree fun_end fun_rec;;

    (*
    let rec tree_alphabeta i =
        match i with
                3 -> ()
                |i ->   let a = alphabetarec maxdepth 1 l.(i) !m infinity true player in
                        if !m < a then begin m:= a; chosen_game:= i end;
                        tree_alphabeta (i+1);                 
    in
    tree_alphabeta 0;
    l.(!chosen_game);; (*return the chosen game*) *)
