open Types
open Fonctions
open Fonctions_jeu
open Alphabeta

(* This function is used to let a human impersonate an animal
   g : a game *)
let rec human g =
     Printf.printf "Choose a movement : \n1(on your left) \n2(straight ahead) \n3(on your right)   \n\n";
     try
      let choice = read_int () in
      Printf.printf "You typed: %d\n" choice;
      match choice with
        1 -> update_game g 2
      | 2 -> update_game g 1
      | 3 -> update_game g 0
      | _ -> begin
                Printf.printf "Impossible choice, try again\n";
                human g;
              end
    with
      _ -> begin
          Printf.printf "Impossible choice, not an integer, try again\n";
          human g;
         end;;

(*Function for asking the player which animal he wishes to play as*)
let rec choose_player () =
  Printf.printf "Choose your player:\nPress 0 for cat\nPress 1 for mouse\n ";
  try
    let choice = read_int () in
    Printf.printf "You typed: %d\n" choice;
     match choice with
       1 -> 1
     | 0 -> 0
     |_ -> begin
            Printf.printf "Impossible choice, try again\n";
            choose_player ();
           end
  with
    _ -> begin
          Printf.printf "Impossible choice, try again\n";
          choose_player ();
         end;;

(*Function for playing a turn in a human vs ia case *)
let turn_play turn human_player _game =
    match turn with
        turn when (turn mod 2) = human_player -> begin
                                                    _game := human !_game;
                                                end
            |_ -> begin
                    _game := alphabeta !_game;
                  end;;

let register_game human_player human_coordinates_list ia_coordinates_list =
  match human_player with
    0-> begin
          save_game !human_coordinates_list "Cat_coordinates.txt";
          save_game !ia_coordinates_list "Mouse_coordinates.txt"
        end
    |_-> begin
          save_game !human_coordinates_list "Mouse_coordinates.txt";
          save_game !ia_coordinates_list "Cat_coordinates.txt"
        end;;