open Types;;
open Fonctions;;


(* creates a new rectangle which boundaries take into account the animal's reach, so that when the center of gravity of the latter hits
   a point of the new rectangle's borders, every points of the bowl representing the animal stay inside the game's rectangle *)
let adapt_environment env ani =
  {pt00 = {x = env.pt00.x +. ani.reach ; y = env.pt00.y +. ani.reach};
   pt01 = {x = env.pt01.x +. ani.reach ; y = env.pt01.y -. ani.reach};
   pt11 = {x = env.pt11.x -. ani.reach ; y = env.pt11.y -. ani.reach};
   pt10 = {x = env.pt10.x -. ani.reach ; y = env.pt10.y +. ani.reach}};;


(* pushes the animal by dist in the direction given by orientation *)
let push_animal ani orientation dist =
  {x = ani.position.x +. dist *. cos(deg_to_rad orientation) ; y = ani.position.y +. dist *. sin(deg_to_rad orientation)};;


(* WARNING : only valid if the goal is the left border *)
(* for the mouse ; checks whether the new position "pos" leads to crossing the goal, ie. if the mouse wins
   pos : a position
   goal : the game's goal *)
let reached_goal pos goal =
  pos.x <= goal.pt1.x;;


(* updates the current player
   g : a game *)
let next_player g = 
    match !(g.current_player) with
      0 -> 1
    | _ -> 0;;


(* makes a copy of the given game
   g : the game to be copied *)
let copy_game g = 
    let copied_game = {g with animals = Array.copy g.animals; current_player = ref !(g.current_player); winner = ref !(g.winner)} in
    copied_game;;


(* checks whether the game ends or not, ie. if one of the players win, when the given animal moves to the given next position ;
   returns the index of the winner, or -1 if none wins after this move
   ani : an animal
   ani_orientation : the animal's orientation
   next_pos : the animal's next position
   g : a game *)
let game_end ani ani_orientation next_pos g =
    match !(g.current_player) with
      0 -> begin
              let res = interception ani.position next_pos g.animals.(1).position (ani.reach +. g.animals.(1).reach) ani.speed in
              match res with
                false -> -1
              | _ -> 0
            end
    | _ -> begin
              let res1 = interception ani.position next_pos g.animals.(0).position (ani.reach +. g.animals.(0).reach) ani.speed in
              let res2 = reached_goal next_pos g.goal in
              match res1, res2 with
                false, false -> -1
              | false, true -> 1
              | _, _ -> 0
            end;;


(* returns the new configuration of the given animal when it aims for a new position ;
   if it collides with a border, it stops upon impact and its new orientation is the symetric of the one it had, relative to the normal vector to the border
   it rebounds on
   ani : an animal
   final_pos : the position the animal is heading towards
   env : the modified environment (computed by the "adapt_environment" function) *)
let calc_final_state ani final_pos env =
    let max_x = env.pt10.x in
    let max_y = env.pt11.y in
    let min_x = env.pt00.x in
    let min_y = env.pt00.y in
    let old_orientation = ani.orientation in
    let new_orientation = ref 0. in
    let new_position = ref {x=0.; y=0.} in
    let p = ani.position in
    let f = final_pos in
    if f.x >= max_x && f.y >= max_y then begin new_orientation := 225.; new_position := {x=max_x; y=max_y} end
    else if f.x >= max_x && f.y <= min_y then begin new_orientation := 135.; new_position := {x=max_x; y=min_y} end
    else if f.x <= min_x && f.y >= max_y then begin new_orientation := 315.; new_position := {x=min_x; y=max_y} end
    else if f.x <= min_x && f.y <= min_y then begin new_orientation := 45.; new_position := {x=min_x; y=min_y} end
    else if f.x >= max_x then begin new_orientation := float_of_int (int_of_float (180. -. old_orientation) mod 360) ; if f.x = p.x then new_position := f else new_position := {x=max_x; y=p.y +. (f.y-.p.y) *. (max_x-.p.x )/. (f.x-.p.x)} end
    else if f.x <= min_x then
        begin
            new_orientation := float_of_int (int_of_float (180. -. old_orientation) mod 360);
            if f.x = p.x then new_position := f else new_position := {x=min_x; y=f.y +. (min_x-.f.x)*.(p.y -. f.y) /. (p.x-.f.x)}
        end (*new_position := {x=min_x; y=p.y +. (f.y-.p.y)*.(p.x -. min_x) /. (f.x-.p.x)}*)
    else if f.y >= max_y then begin new_orientation := float_of_int (int_of_float (-. old_orientation) mod 360) ; if f.y = p.y then new_position := f else new_position := {x=p.x +. (f.x -. p.x)*.(max_y -. p.y) /. (f.y -. p.y); y=max_y} end
    else begin new_orientation := float_of_int (int_of_float (-. old_orientation) mod 360) ; if f.y = p.y then new_position := f else new_position := {x=p.x +. (f.x -. p.x)*.(p.y -. min_y) /. (p.y -. f.y); y=min_y} end;
    {ani with position = !new_position; orientation = !new_orientation};;


(* updates the game when an animal makes a move and changes the current_player if none won after that move ;
   var's values are 0, 1 or 2 (returned upon the creation of an array, see Array.init in the "possible_games" function for more details) and are used
   to represent one ot the 3 possible directions the animal can, take (0 for the right one, 1 for the middle one and 2 for the left one - "left" and "right" are to
   be understood from the animal's perspective) ; they are converted to -1, 0 or 1 for an easier use through var2
   g : a game *)
let update_game g var =
    let adapted_env = adapt_environment g.environment g.animals.(!(g.current_player)) in
    let copied_game = copy_game g in
    let var2 = float_of_int (var - 1) in
    let ani = copied_game.animals.(!(copied_game.current_player)) in
    let new_orientation = float_of_int ((int_of_float (ani.orientation +. var2 *. ani.angle)) mod 360) in
    let new_pos = push_animal ani new_orientation ani.speed in
    if is_in new_pos adapted_env
    then
        copied_game.animals.(!(copied_game.current_player)) <- {ani with position = new_pos; orientation = new_orientation}
    else
        copied_game.animals.(!(copied_game.current_player)) <- calc_final_state ani new_pos adapted_env;
    copied_game.winner := game_end ani new_orientation new_pos copied_game;
    copied_game.current_player := next_player copied_game;
    copied_game;;


(* returns an array with the three possible games outcomes when an animal has to make a move
   g : a game *)
let possible_games g =
    Array.init 3 (update_game g);;


(* returns the number of weights that will have to be modified for the given pool
   pool : an array of animals of the same heuristic formula *)
let nb_weights ani =
  let choosen_h = ani.heuristic in
  match choosen_h with
    4 | 9 -> 2
  | 5 | 6 | 10  -> 3
  | 11 -> 4
  | _ -> 0


(* distributes weights to the pool's animals' heuristics in order to generate a set of ia sufficiently diverse in terms of heuristics
   to start an optimization process
   pool : an array of animals of the same heuristic formula *)
let distribute_weights pool =
    let nb_w = nb_weights pool.(0) in
    match nb_w with
      2 -> begin
              let step = 1. /. (float_of_int (Array.length pool) -. 1.) in
              let rec aux i =
                match i with
                  i when i=(Array.length pool) -> pool
                  |_ -> begin
                          let w1 = (float_of_int i)*.step in
                          let w2 = 1.-.w1 in
                          let w3 = 0. in
                          pool.(i)<-{pool.(0) with weight_h = [|w1;w2;w3|]};
                          aux (i+1);
                        end
              in
              aux 0;
          end
    | 3 -> begin
              let borne = int_of_float ((float_of_int (Array.length pool)) ** 0.5) in
              let step = 1. /. (float_of_int (borne) -. 1.) in
              let new_pool = Array.make (borne*(borne+1)/2) pool.(0) in
              let rec aux i j compteur =
                match i,j,compteur with
                  i, j, _ when i=borne-1 -> new_pool
                | i, j, compteur when j=borne-i -> aux (i+1) 0 compteur
                | i, j, compteur -> begin
                                        let w1 = (float_of_int i)*.step in
                                        let w2 = (float_of_int j)*.step in
                                        let w3 = 1.-.w1-.w2 in
                                        new_pool.(compteur)<-{pool.(0) with weight_h = [|w1;w2;w3|]};
                                        aux i (j+1) (compteur+1)
                                    end
              in
              aux 0 0 0;
           end
     | _ -> failwith "Wrong number of weights (must be 2 or 3)\n"



(* generates an array of ia of the same heuristic ;
   in nb_cX and nb_mX, the X represents the id of the used heuristic (see the "heuristic_match" function in the heur.ml module for correspondance)
   nb_cX : the number of cats with the heuristic X
   nb_mX : the number of mouses with the heuristic X *)
let create_pool nb_c4 nb_c5 nb_c6 nb_m9 nb_m10 =
  let cat_std_4 = {standard_cat with heuristic = 4} in
  let cat_std_5 = {standard_cat with heuristic = 5} in
  let cat_std_6 = {standard_cat with heuristic = 6} in
  let mouse_std_9 = {standard_mouse with heuristic = 9} in
  let mouse_std_10 = {standard_mouse with heuristic = 10} in
  let pool_cats_4 = Array.make (nb_c4) cat_std_4 in
  let pool_cats_5 = Array.make (nb_c5) cat_std_5 in
  let pool_cats_6 = Array.make (nb_c6) cat_std_6 in
  let pool_mouses_9 = Array.make (nb_m9) mouse_std_9 in
  let pool_mouses_10 = Array.make (nb_m10) mouse_std_10 in
  let list_pool_cats = [distribute_weights pool_cats_4; distribute_weights pool_cats_5; distribute_weights pool_cats_6] in
  let list_pool_mouses = [distribute_weights pool_mouses_9; distribute_weights pool_mouses_10] in
  let pool_cats = Array.concat list_pool_cats in
  let pool_mouses = Array.concat list_pool_mouses in
  (pool_cats,pool_mouses);;



let print_animal ani =
  (*
  Printf.printf "Position [%f, %f] " ani.position.x ani.position.y;
  Printf.printf "Orientation: %f, Speed: %f, Reach: %f " ani.orientation ani.speed ani.reach;
  Printf.printf "Angle: %f, Heuristic: %d " ani.angle ani.heuristic;
  print_float_array ani.weight_h;;
  *)
  if Array.length ani.weight_h = 3 then
  Printf.printf "{position= {x=%f; y=%f}; orientation= %f; speed= %f; reach= %f; angle= %f; heuristic= %d; weight_h= [| %f; %f; %f|]} ;\n" ani.position.x ani.position.y ani.orientation ani.speed ani.reach ani.angle ani.heuristic ani.weight_h.(0) ani.weight_h.(1) ani.weight_h.(2)
  else Printf.printf "{position= {x=%f; y=%f}; orientation= %f; speed= %f; reach= %f; angle= %f; heuristic= %d; weight_h= [| %f; %f; %f; %f|]} ;\n" ani.position.x ani.position.y ani.orientation ani.speed ani.reach ani.angle ani.heuristic ani.weight_h.(0) ani.weight_h.(1) ani.weight_h.(2) ani.weight_h.(3);;
