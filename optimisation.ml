open Array
open Types
open Fonctions
open Alphabeta
open Fonctions_jeu

(*Plays a game from start to finish and returns the winner, or -1 if the game is longer than 200 turns and ends in a draw*)
let play_game rect goal chat souris = 
  let _game = ref (create_game souris chat rect goal) in
  let rec main_rec win turn =
      match win,turn  with
              -1,turn when turn<200 -> begin
                      _game := alphabeta !_game;
                      main_rec !(!_game.winner) (turn+1);
                    end
              |_ -> ()
  in
  main_rec !(!_game.winner) 0;
  !(!_game.winner);;


(*function to find a mouse such that it has approximatively 50% winrate against the cat given as a parameter
This is useful for optimization later, because it is easier to analyze the data when starting from a 50/50 winrate
This function does put a series of "dumb" mouses with different parameters against a cat , with 14 different starting positions for the mouse
The mouses are considered "dumb" because their heuristic is only based on their distance to the goal, nothing else*)
let finding_fifty cat = 
  let mouse_positions = [| {x=750.; y=50.}; {x=750.; y=200.}; {x=750.; y=400.}; {x=750.; y=600.}; {x=750.; y=750.}; {x=650.; y=100.}; {x=650.; y=300.}; {x=650.; y=500.}; {x=650.; y=700.}; {x=500.; y=50.}; {x=500.; y=200.}; {x=500.; y=400.}; {x=500.; y=600.}; {x=500.; y=750.};|]in
  let mouse_orientations = [|135.; 180.; 215.|] in
  let mouse_reaches = [|5.; 10.; 15.; 20.; 25.; 30.; 35.; 40.|] in
  let mouse_speeds = [|5.; 10.; 15.; 20.; 25.; 30.; 35.; 40.|] in
  let mouse_angles = [|10.; 15.; 20.; 25.; 30.; 35.; 40.; 45.|] in
  let total = length mouse_positions * length mouse_orientations in
  let lmax = length mouse_speeds in
  let mmax = length mouse_angles in
  let imax = length mouse_positions in
  let jmax = length mouse_orientations in
  let kmax = length mouse_reaches in
  let rec aux_rec l m k i j accum =
      match l,m,k,i,j,accum with
          l,_,_,_,_,_ when l=lmax -> ()
          |l,m,_,_,_,_ when m=mmax -> aux_rec (l+1) 0 0 0 0 0
          |l,m,k,_,_,_ when k=kmax -> aux_rec l (m+1) 0 0 0 0 
          |l,m,k,i,_,accum when i=imax -> begin
                              let mouse_winrate = (float_of_int accum /. float_of_int total) in
                              Printf.printf "speed: %f , angle: %f , reach: %f , mouse %f\n" mouse_speeds.(l) mouse_angles.(m) mouse_reaches.(k) mouse_winrate;
                              flush stdout;
                              aux_rec l m (k+1) 0 0 0
                           end 
          |l,m,k,i,j,accum when j=jmax-> aux_rec l m k (i+1) 0 accum
          |l,m,k,i,j,accum -> begin
                              let souris = (create_animal mouse_positions.(i) mouse_orientations.(j) mouse_speeds.(l) mouse_reaches.(k) mouse_angles.(m) 6 [| 0.; 0.; 0.|]) in
                              let win = play_game rect goal cat souris in
                              if win = 1 then aux_rec l m k i (j+1) (accum+1) else aux_rec l m k i (j+1) accum
                            end
  in
  aux_rec 0 0 0 0 0 0;;


(*function that puts all cats against all mouses and returns the number of duels won for each of them*)
let showdown cats mouses =
  (*Defining all the constants we are going to need *)
  let mouse_positions = [| {x=750.; y=400.}; {x=750.; y=750.}; {x=600.; y=700.}|] in
  let mouse_orientations = [| 180.; 225.; 270. |] in
  let cat_positions = [| {x=50.; y=400.}; {x=50.; y=750.}; {x=200.; y=150.} |] in
  let cat_orientations = [| 0.; 315.; 90. |] in
  let total_duels_per_cat = length mouses * length cat_positions * length mouse_positions in
  let cat_win = make (length cats) 0 in
  let nb_null_cat = make (length cats) 0 in
  let mouse_win = make (length mouses) 0 in
  let imax = length cats in
  let jmax = length mouses in
  let kmax = length cat_positions in
  let lmax = length mouse_positions in
  (*Defining the functions that will be needed for the recursion calls *)
  let fun_rec_l i j k l =
    let cat =  {cats.(i) with position = cat_positions.(k); orientation = cat_orientations.(k)} in
    let mouse =  {mouses.(j) with position = mouse_positions.(l); orientation = mouse_orientations.(l)} in
    let win = play_game rect goal cat mouse in
    if win = 0 then cat_win.(i) <- cat_win.(i) + 1 else if win = 1 then mouse_win.(j) <- mouse_win.(j) + 1 else nb_null_cat.(i) <- nb_null_cat.(i) + 1
  in
  let fun_limit_j i =
    Printf.printf "Cat %d : winrate %f null %f\n" (i+1) (float_of_int cat_win.(i) /. float_of_int total_duels_per_cat) (float_of_int nb_null_cat.(i) /. float_of_int total_duels_per_cat);
    flush stdout
  in
  (*Redefining the recursion function there because it is the only function that uses 4 arguments*)
  let rec showdown_rec i j k l =
    match i,j,k,l with
      i,_,_,_ when i=imax -> ()
      |i,j,_,_ when j=jmax -> begin
                                fun_limit_j i;
                                showdown_rec (i+1) 0 0 0
                              end
      |i,j,k,_ when k=kmax -> showdown_rec i (j+1) 0 0
      |i,j,k,l when l=lmax -> showdown_rec i j (k+1) 0
      |i,j,k,l -> begin
                    fun_rec_l i j k l;
                    showdown_rec i j k (l+1)
                  end
  in
  showdown_rec 0 0 0 0;
  (cat_win, mouse_win);;


(*Returns the list of the best animals, with ratio the dividing factor of the list length*)
let selection_best ani_array score_array ratio = 
  let l = Array.length score_array in
  let l2 = l / ratio in
  let best_array = Array.init l2 (fun i -> i) in
  let rec aux_rec i j b =
	match i,j,b with
		i,_,_ when i = l -> ()
		|i,j,b when (j>=l2 || b=false) -> aux_rec (i+1) 0 true
		|i,j,b -> begin
				if score_array.(i) > score_array.(best_array.(j)) then
				begin
					best_array.(j) <- i;
					aux_rec i (j+1) false
				end
				else
					aux_rec i (j+1) true;
			 end
  in
  aux_rec l2 0 true;
  let best_ani_array = Array.make l2 ani_array.(best_array.(0)) in
  let limit = l2 in
  let start = 1 in
  let fun_limit arg = () in
  let fun_rec arg = best_ani_array.(arg) <- ani_array.(best_array.(arg)) in
  iter1 start limit fun_limit fun_rec;
  (*Here, we stock the best animals in their best_array array*)
  best_ani_array


(*Returns an array of mutated animals, each animal in the ani_array being mutated on ratio different instances, each weight being changed by maximum max_change*)
let mutation ani_array ratio max_change =
  let finish = Array.length ani_array in
  let l2 = finish * ratio in
  let new_generation = Array.make l2 ani_array.(0) in
  let start = 0 in
  let fun_limit i = new_generation in (*finish case here, we return the newly created generation *)
  let fun_rec i =
    let start = 0 in
    let finish = ratio in
    let fun_limit j = () in
    let ani = ani_array.(i) in
    let fun_rec_internal j = 
      let w1 = abs_float(ani.weight_h.(0) +. (Random.float (2. *. max_change)) -.max_change) in
      let w2 = abs_float(ani.weight_h.(1) +. (Random.float (2. *. max_change)) -.max_change) in
      if (nb_weights ani) = 0 then 
        new_generation.(i * ratio + j) <- ani
      else if (nb_weights ani) = 2 then
      begin
        let r = w1 +. w2 in
        new_generation.(i * ratio + j) <- {ani with weight_h = [|w1 /. r; w2 /. r; 0.|]};  (* To get the sum of the coefficients to be equal to 1*)
      end
      else 
      begin
        let w3 = abs_float(ani.weight_h.(2) +. (Random.float (2. *.max_change)) -.max_change) in
        let r = w1 +. w2 +. w3 in
        new_generation.(i * ratio + j) <- {ani with weight_h = [|w1 /. r; w2 /. r; w3 /. r|]}; (* To get the sum of the coefficients to be equal to 1*)
      end
    in
    iter1 start finish fun_limit fun_rec_internal;
  in
  iter1 start finish fun_limit fun_rec;;


(*Function that does number_generations times a showdown and a mutation with the ratio parameter and prints and returns the best cats and mouses found*)
let evolution cat_array mouse_array number_generations ratio max_change = 

  let cats = ref (Array.copy cat_array) in
  let mouses = ref (Array.copy mouse_array) in

  let start = 0 in
  let finish = number_generations in
  let fun_limit i = (cats,mouses) in
  let fun_rec_internal i best_animals =
    let start = 0 in
    let finish = Array.length best_animals in
    let fun_limit j = () in
    let fun_rec j = print_animal best_animals.(j) in
    iter1 start finish fun_limit fun_rec
  in
  let fun_rec i =
      let cat_win,mouse_win = showdown !cats !mouses in
      print_int_array cat_win;
      print_int_array mouse_win;
      let best_cats  = selection_best !cats cat_win ratio in
      let best_mouses = selection_best !mouses mouse_win ratio in

      Printf.printf "Generation %d best cats:\n" i;
      fun_rec_internal i best_cats;
      Printf.printf "Generation %d best mouses:\n" i;
      fun_rec_internal i best_mouses;

      cats := mutation best_cats ratio max_change;
      mouses := mutation best_mouses ratio max_change;

      Printf.printf "It is time for a new generation, even stronger than the old one\n"
  in
  iter1 start finish fun_limit fun_rec;;
