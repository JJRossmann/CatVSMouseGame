val adapt_environment : Types.rectangle -> Types.animal -> Types.rectangle
(*Returns the adapted field of an Animal *)

val push_animal : Types.animal -> float -> float -> Types.point
(*Returns the final position of an Animal by giving it a distance and an orientation *)

val reached_goal : Types.point -> Types.line -> bool
(*Tests if the point is on or beyond the line goal *)

val next_player : Types.game -> int
(*Returns the next player of the game *)

val copy_game : Types.game -> Types.game
(*Returns a deep copy of the game *)

val game_end : Types.animal -> 'a -> Types.point -> Types.game -> int
(*Checks if the game ends, returns the winner (or -1 if there isn't any winner) *)

val calc_final_state :
  Types.animal -> Types.point -> Types.rectangle -> Types.animal
(*Returns the animal after its move *)

val update_game : Types.game -> int -> Types.game
(*Returns the updated game after the current player chose the move (given by an int) *)

val possible_games : Types.game -> Types.game array
(*Returns an array of the possible states of the game next turn *)

val nb_weights : Types.animal -> int
(*Returns the number of useful weights of the animal heuristic *)

val distribute_weights : Types.animal array -> Types.animal array
(*Returns the animal array with distributed weights among each animals *)

val create_pool :
  int -> int -> int -> int -> int -> Types.animal array * Types.animal array
(*Creates the mice and cats pools for the genetic algorithm *)

val print_animal : Types.animal -> unit
(*Prints the animal on the standard output *)