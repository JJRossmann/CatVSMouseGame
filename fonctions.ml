open Types
open Printf



(*Abstract iter function *)
let iter1 start limit fun_limit fun_rec =
	let rec iter_rec arg =
		match arg with
			arg when arg=limit -> fun_limit arg
			|arg -> begin
					fun_rec arg;
					iter_rec (arg+1)
				end
	in
	iter_rec start;;


let create_animal pos orient v p a_dep h weights = {position=pos; orientation=orient; speed=v; reach=p; angle=a_dep; heuristic=h; weight_h=weights};;
(*Returns an animal with the given parameters*)


let create_game mouse cat field mouse_goal = {animals= [| cat; mouse |]; nb_cats= 1; nb_mouses= 1; environment= field; goal= mouse_goal; current_player = ref 0; winner = ref (-1)};;
(*Returns a game with the given parameters*)


let deg_to_rad d = d *. pi /. 180. ;;
(*Function to convert degree to radians *)

let rad_to_deg r = r *. 180. /. pi ;;
(*Fonction to convert radians to degrees *)


let distance p1 p2 = sqrt ((p1.x -. p2.x)**2. +. (p1.y -. p2.y)**2.);;
(*Calculates the distance between two Points*)


let diff_cat_mouse_orientation cat mouse = abs_float cat.orientation -. mouse.orientation;;
(*Gives the difference of the orientations of a cat and a mouse*)


let is_in pt rect =
    if pt.x < rect.pt11.x && pt.x > rect.pt00.x && pt.y < rect.pt11.y && pt.y > rect.pt00.y
    then
    	true
    else
    	false;;
(*Tells if the point is in the given rectangle*)


let interception pt1 pt2 pto reach speed = 
  (*Tests if the point pto is at an inferior distance than reach from the pt1-pt2 segment and if the projection of pto is within the given speed*)
  let a = (pt2.y -. pt1.y) /. (pt2.x -. pt1.x) in (*slope coefficient of the pt1-pt2 segment*)
  let b = pt1.y -. a *. pt1.x in (*find b in the equation y=ax+b where ax-y+b=0*)
  let c = -. pto.x -. a *. pto.y in (*find c in the equation x+ay+c=0 passing through pto and its projection on (pt1,pt2)*)
  let projection = {x = -. (a *. b +. c) /. (a *. a +. 1.); y = (b -. a *. c) /. (a *. a +. 1.)} in (*coordonitates of the projection of pto*)
  
  let d = distance pto projection in
  if d > reach (*checking the distance between pto and (pt1,pt2)*)
    then false
  else if not (distance pt1 pto < speed && distance pt2 pto < speed) (*checking if the projection of pto is outside [pt1,pt2]*)
    then 
      if distance pt2 pto > reach (*checking if, when outside, we aren't in reach of pt2*)
        then false 
        else true
  else true;; (*if not oustide, then automaticly in reach (per first if)*)


let refresh_coordinates_list coordinates_list_ref g  =
  let current_player = g.current_player in
  coordinates_list_ref := g.animals.(!current_player).position::(!coordinates_list_ref);;


let save_game coordinates_animal filename =
  (*From a list of the coordinates an animal had during a game, writes them in a file *)
    let oc = open_out filename in (*opens the file *)
    let rec save_rec l =
        match l with
            []->()
            |hd::tl -> begin
                        save_rec tl;
                        fprintf oc "%f %f\n" hd.x hd.y; (*writes the coordinates in the file*)
                        flush oc; 
                        end
    in
    save_rec coordinates_animal;
    close_out oc;; (*closes the file *)


let save_animals_winrate ani_array winrate_array filename =
(*From an animal array and their winrates, writes in a file each animal's winrate with their AI parameters *)
    let oc = open_out filename in
    let rec fprintf_array nh i j =
    (*Function for writing the weights from the heuristic weights array in the file. Requires an additional argument, hence its existence. *)
        match i with
            i when i=nh -> () (*nh indicates when the array stops *)
            |i -> begin
                    fprintf oc "%f " ani_array.(j).weight_h.(i); (*j is the numero of the current animal we are writing its weights from*)
                    fprintf_array nh (i+1) j
                  end
    in
    let start = 0 in
    let finish = Array.length ani_array in
    let fun_limit j = close_out oc in
    let fun_rec j = 
      fprintf oc "N°%d, winrate:%f, choosen heuristic:%d, weights: " j winrate_array.(j) ani_array.(j).heuristic; (*writes the numero of the animal, its winrate, heuristic*)
      let n_heur = Array.length ani_array.(j).weight_h in
      fprintf_array n_heur 0 j; (*writes the weights array*)
      fprintf oc "\n"; (*next line before the new call*)
      flush oc
    in
    iter1 start finish fun_limit fun_rec;;


(*Prints an array of floats*)
let print_float_array arr = 
  printf "[ ";
  let limit = -1 + Array.length arr in
  let fun_limit arg = printf " %f ]\n" arr.(arg) in
  let fun_rec arg = printf "%f , " arr.(arg) in
  iter1 0 limit fun_limit fun_rec;;

(*Prints an Array of ints*)
let print_int_array arr = 
  printf "[ ";
  let limit = -1 + Array.length arr in
  let fun_limit arg = printf " %d ]\n" arr.(arg) in
  let fun_rec arg = printf "%d , " arr.(arg) in
  iter1 0 limit fun_limit fun_rec;;


(*Print the weights arrays of the animals of the ani_array*)
let print_weights ani_array i n =
  let limit = n in
  let fun_limit arg = () in
  let fun_rec arg = print_float_array ani_array.(arg).weight_h in
  iter1 0 limit fun_limit fun_rec;;
