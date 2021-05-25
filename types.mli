type point = { x : float; y : float; }
(*Point object : two float coordinates *)

type animal = {
  position : point;
  orientation : float;
  speed : float;
  reach : float;
  angle : float;
  heuristic : int;
  weight_h : float array;
}
(*Animal object : a position given by a point, an orientation, speed, reach and angle given by floats, an heuristic given by an int, an array of the weights of its heuristic *)

type rectangle = { pt00 : point; pt10 : point; pt11 : point; pt01 : point; }
(*Rectangle object for the game field, given by its 4 vertices *)

type line = { pt1 : point; pt2 : point; }
(*Line object, mainly for the goal, given by two points *)

type game = {
  animals : animal array;
  nb_cats : int;
  nb_mouses : int;
  environment : rectangle;
  goal : line;
  current_player : int ref;
  winner : int ref;
}
(*Game object : an array of the animals playing, a number of cats and mice, a field given by a rectange, a line goal, a ref on the current player and on the winner. *)

val pi : float
val width : float
val height : float
val ppt00 : point
val ppt01 : point
val ppt10 : point
val ppt11 : point
val rect : rectangle
val goal : line
val maxdepth : int
val standard_cat : animal
val standard_mouse : animal
(*Global constants used throughout the various modules *)
