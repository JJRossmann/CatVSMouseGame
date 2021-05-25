open Types;;
open Fonctions;;
open Fonctions_jeu;;
open Graphics;;
(*Before using these functions, be sure there is a line of code 'Graphics.open_graph ""' before calling them.*)



let draw_environment g =
  (*Draws the rectangle of the game. It also clears the graphic window because it draws again the rectangle*)
  let width = int_of_float (distance g.environment.pt00 g.environment.pt10) in
  let height = int_of_float (distance g.environment.pt00 g.environment.pt01) in
  resize_window width height; (*Centers the display window on the rectangle*)
  let x=int_of_float g.environment.pt00.x in
  let y=int_of_float g.environment.pt00.y in
  set_color black; (*borders of the field in black*)
  draw_rect x y width height;;


let move_animal old_coos new_coos =
  (*Draws the movement of an animal through its old and new coordinates*)
  moveto (int_of_float old_coos.x) (int_of_float old_coos.y); (*initialize the current point of the graphic*)
  lineto (int_of_float new_coos.x) (int_of_float new_coos.y);; (*moves the current point and draws the line between the old coordinates and the new ones*)


let rec draw_list_coos coos_animal =
  (*Draws the movements of the animals that are in the list coos_animal *)
  match coos_animal with
    []->failwith "Empty coordinates list\n"
    |[x]->()
    |x::y::tl -> begin
                  move_animal y x;
                  draw_list_coos (y::tl)
                 end;;


let draw_possibilities ani = 
  (*Draws the possible movements of an animal*)
  let new_point_0 = push_animal ani ani.orientation ani.speed in
  let new_point_1 = push_animal ani (ani.orientation+.ani.angle) ani.speed in
  let new_point_2 = push_animal ani (ani.orientation-.ani.angle) ani.speed in (*the three final possible positions of the animal*)
  let point_ini = ani.position in 
  set_color blue;
  move_animal point_ini new_point_0;
  move_animal point_ini new_point_1;
  move_animal point_ini new_point_2;;


let draw_animal ani =
  (*Draws a representation of the animal *)
  let x = int_of_float ani.position.x in
  let y = int_of_float ani.position.y in
  let r = int_of_float ani.reach in
  set_color black;
  plot x y; (*Draws the position of the animal *)
  set_color red;
  draw_circle x y r (*Draws the reach of the animal *);;


let draw_state_game g =
  (*Draws the animals in the game with their possibilities. Does NOT draw the environment of the game*)
  let start = 0 in
  let finish = (g.nb_cats + g.nb_mouses) in
  let fun_limit i = () in
  let fun_rec i =
    draw_animal g.animals.(i);
    draw_possibilities g.animals.(i)
  in
  iter1 start finish fun_limit fun_rec;;
  (*
  let rec draw_animals i =
    (*Function for drawing recursively each animal one by one*)
    match i with
      0 -> ()
    |i -> draw_animal g.animals.(i-1);
        draw_possibilities g.animals.(i-1);
        draw_animals (i-1);
  in
  draw_animals (g.nb_cats + g.nb_mouses);;*)


let show_game g =
  (*Draws the game with its environment*)
  draw_environment g;
  draw_state_game g;;


let show_final_state_game g coos_cat coos_mouse =
  (*Draws the final state of the game, with all the coordinates the mouse and the cat went to*)
  show_game g;
  set_color magenta;
  draw_list_coos coos_cat;
  set_color cyan;
  draw_list_coos coos_mouse;;

let press_mouse_to_next_screen () =
  (*Function to wait for the mouse to be pressed before moving to the next screen*)
  let s = wait_next_event [Button_down] in
  match s with
    _ -> ();





  
  
    
  






  
    




