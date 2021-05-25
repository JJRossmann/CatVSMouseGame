open Types
open Fonctions
open Fonctions_jeu
open Alphabeta
open Graph
open Versus

let main rect goal =
    (*Initializing the game *)
    let _game = ref (create_game standard_mouse standard_cat rect goal) in
    let human_player = choose_player () in
    let ia_player = (human_player +1) mod 2 in

    let human_coordinates_list = ref [!_game.animals.(human_player).position] in
    let ia_coordinates_list = ref [!_game.animals.(ia_player).position] in

    Printf.printf "M [%f %f] [%f %f]\n" !_game.animals.(0).position.x !_game.animals.(0).position.y !_game.animals.(1).position.x !_game.animals.(1).position.y;
    Graphics.open_graph "";
    show_game !_game;

    let fun_rec win turn =
        Printf.printf "M Tour %d player: %d\n" turn !(!_game.current_player);
        turn_play turn human_player _game;
        if (turn mod 2)=human_player then refresh_coordinates_list ia_coordinates_list !_game else refresh_coordinates_list human_coordinates_list !_game;
        Printf.printf "M [%f %f] [%f %f] player: %d\n" !_game.animals.(0).position.x !_game.animals.(0).position.y !_game.animals.(1).position.x !_game.animals.(1).position.y !(!_game.current_player);
        flush stdout;
        show_game !_game;
    in
    let rec main_rec win turn =
        match win with
                -1 -> begin
                        fun_rec win turn;
                        main_rec !(!_game.winner) (turn+1);
                      end
                |_ -> ()
    in
    main_rec !(!_game.winner) 0; (*We play the game *)

    (*Once the game is finished, we show how it went by drawing the movements of both players *)
    Printf.printf "winner : %d\n" !(!_game.winner) ;
    show_final_state_game !_game !human_coordinates_list !ia_coordinates_list;
    press_mouse_to_next_screen ();

    (*Finally, we register the coordinates in a txt file.*)
    register_game human_player human_coordinates_list ia_coordinates_list;;


main rect goal;;