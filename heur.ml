open Types
open Fonctions
open Fonctions_jeu


(* Compute the orientation of the mouse with respect to the orientation pointing to the goal (arrive line) *)
let mouse_goal_orientation mouse = abs_float 180. -. mouse.orientation;;

let relative_animal_orientation ani1 ani2 =
    let pt_cat = ani1.position in
    let pt_mouse = ani2.position in
    let pt_cat_orient = push_animal ani1 ani1.orientation ani1.speed in
    let u = {x=pt_cat_orient.x -. pt_cat.x; y=pt_cat_orient.y -. pt_cat.y} in
    let v = {x=pt_mouse.x -. pt_cat.x; y=pt_mouse.y -. pt_cat.y} in
    let u_scal_v = u.x *. v.x +. u.y *. v.y in
    let len_u = distance pt_cat_orient pt_cat in
    let len_v = distance pt_mouse pt_cat in
    let theta = rad_to_deg (acos (u_scal_v /. (len_u *. len_v)) ) in
    theta;;


(* Minimize the distance between the cat and the mouse *)
let h1_cat _game = let d = distance _game.animals.(1).position _game.animals.(0).position in 1. -. (d /. width);;


(* Minimize the distance between the cat and the half-point on the segment between the mouse and the goal =~ "intercept the mouse"*)
let h2_cat _game = let d = distance _game.animals.(1).position {x=_game.animals.(0).position.x /. 2.; y=_game.animals.(1).position.y} in 1. -. (d /. width);;


(* Maximize the distance between the mouse and the goal*)
let h3_cat _game =  let d = _game.animals.(1).position.x /. width in (d /. width) -. 1.;;


(* Minimize the distance between the cat and the mouse and minimize the difference of the orientation between the two animals with respective weights*)
let h4_cat _game = let weights = _game.animals.(0).weight_h in  
                    let diff = relative_animal_orientation _game.animals.(0) _game.animals.(1) in
                    let orient = 1. -. (diff /. 180.) in
                    weights.(0) *. h1_cat _game +. weights.(1) *. orient ;;


(* If the mouse is between the cat and the goal, use h2_cat, else use h3_cat, while pointing to the mouse as best as possible *)
let h5_cat _game = let weights = _game.animals.(0).weight_h in 
                       if _game.animals.(0).position.x >= _game.animals.(1).position.x then h2_cat _game
                       else begin
                            let diff = relative_animal_orientation _game.animals.(0) _game.animals.(1) in
                            let orient = 1. -. (diff /. 180.) in
                        weights.(2) *. orient +. (h3_cat _game);
                       end;;


(* Combination of h1_cat, h2_cat and h3_cat *)
let h6_cat _game = let weights = _game.animals.(0).weight_h in if _game.animals.(0).position.x >= _game.animals.(1).position.x then h2_cat _game
                       else
                       begin
                            let diff = relative_animal_orientation _game.animals.(0) _game.animals.(1) in
                            let orient = 1. -. (diff /. 180.)  in
                            weights.(0) *. orient +. weights.(1) *. (h2_cat _game) +. weights.(2) *. (h1_cat _game);
                        end;;



(* Minimize the distance between the mouse and the goal *)
let h1_mouse _game = let d = _game.animals.(1).position.x in 1. -. d /. width;;


(* Maximize the distance between the mouse and the cat *)
let h2_mouse _game = let d = distance _game.animals.(0).position _game.animals.(1).position in (d /. width) -. 1.;;


(* Combination of the two precious heuristics with respective weights *)
let h3_mouse _game = let weights = _game.animals.(1).weight_h in weights.(0) *. h1_mouse _game +. weights.(1) *. h2_mouse _game ;;


(* Combination of h1_mouse, minimizing the orientation of the mouse towards the goal and h2_mouse *)
let h4_mouse _game = 
    let weights = _game.animals.(1).weight_h in
    let orient = 1. -. ( mouse_goal_orientation _game.animals.(1) /. 180. ) in
    weights.(0) *. h1_mouse _game +. weights.(1) *. orient +. weights.(2) *. h2_mouse _game ;;


(* Map the heuristic of an animal with the respective function *)
let heuristic_match _game player= 
    match _game.animals.(player).heuristic with
    | 1 -> h1_cat _game
    | 2 -> h2_cat _game
    | 3 -> h3_cat _game
    | 4 -> h4_cat _game
    | 5 -> h5_cat _game
    | 6 -> h6_cat _game
    | 7 -> h1_mouse _game
    | 8 -> h2_mouse _game
    | 9 -> h3_mouse _game
    | 10 -> h4_mouse _game
    |_ -> failwith "Heuristic does not exist"

