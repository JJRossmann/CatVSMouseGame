type point = {x: float; y: float};;

type animal = {position: point; orientation: float; speed: float; reach: float; angle: float; heuristic: int; weight_h: float array};;

type rectangle = {pt00: point; pt10: point; pt11: point; pt01: point};;

type line = {pt1: point; pt2: point};;

type game = {animals: animal array; nb_cats: int; nb_mouses: int; environment: rectangle; goal: line; current_player: int ref; winner: int ref};;


(*Global constants*)
let pi = acos (-1.);;


(*parameters of the playing area*)
let width = 600.;;
let height = 600.;;
let ppt00 = {x=0.; y=0.};;
let ppt01 = {x=0.; y=height};;
let ppt10 = {x=width; y=0.};;
let ppt11 = {x=width;y=height};;
let rect = {pt00=ppt00; pt01=ppt01; pt10=ppt10; pt11=ppt11};;
let goal = {pt1=ppt00; pt2=ppt01};;


(*AlphaBeta maximum depth*)
let maxdepth = 13;;


(*Standard Cat and mouse for optimization*)
let standard_cat = {position= {x=50.; y=300.}; orientation= 0.; speed= 22.5; reach= 22.5; angle= 15.; heuristic= 6; weight_h= [| 0.61; 0.02; 0.37|]};;
let standard_mouse = {position= {x=550.; y=300.}; orientation= 180.; speed= 26.25; reach= 15.; angle= 26.25; heuristic= 10; weight_h= [| 0.56; 0.03; 0.39|]};;
let standard_cat2 = {position= {x=50.; y=300.}; orientation= 0.; speed= 22.5; reach= 22.5; angle= 15.; heuristic= 4; weight_h= [| 0.97; 0.03; 0.|]};;
let standard_mouse2 = {position= {x=550.; y=300.}; orientation= 180.; speed= 26.25; reach= 15.; angle= 26.25; heuristic= 9; weight_h= [| 0.50; 0.50; 0.|]};;