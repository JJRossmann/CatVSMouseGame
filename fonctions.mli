val iter1 : int -> int -> (int -> 'a) -> (int -> 'b) -> 'a
(*Personnalized abstract iter function *)

val create_animal :
  Types.point ->
  float -> float -> float -> float -> int -> float array -> Types.animal
(*Function for creating animals using its parameters *)

val create_game :
  Types.animal -> Types.animal -> Types.rectangle -> Types.line -> Types.game
(*Creates a game with the given parameters *)

val deg_to_rad : float -> float
(*Converts degrees to radians *)

val rad_to_deg : float -> float
(*Converts radians to degrees *)

val distance : Types.point -> Types.point -> float
(*Calculates the distance between two Points *)

val diff_cat_mouse_orientation : Types.animal -> Types.animal -> float
(*Calculates the orientation difference between two Animals  *)

val is_in : Types.point -> Types.rectangle -> bool
(*Returns true if the Point is in the Rectangle *)

val interception :
  Types.point -> Types.point -> Types.point -> float -> float -> bool
(*Used to know if, during a movement, the cat intercepts the mouse, by using the inital position of the cat, the final position, the poisition of the mouse and the reaches *)

val refresh_coordinates_list : Types.point list ref -> Types.game -> unit
(*Refreches the coordinates list of a game when a turn is played *)

val save_game : Types.point list -> string -> unit
(*Writes the coordinates of the Point list in a file *)

val save_animals_winrate :
  Types.animal array -> float array -> string -> unit
(*Writes the winrates of the animals in a file by using an Animal array and a float array *)

val print_float_array : float array -> unit
val print_int_array : int array -> unit
val print_weights : Types.animal array -> 'a -> int -> unit
(*Functions for printing arrays as we wanted them to be printed *)