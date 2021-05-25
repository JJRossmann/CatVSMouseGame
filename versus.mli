val human : Types.game -> Types.game
(*Returns the game after a human's turn *)

val choose_player : unit -> int
(*Asks which player we want to play as *)

val turn_play : int -> int -> Types.game ref -> unit
(*Updates the game after a turn *)

val register_game :
  int -> Types.point list ref -> Types.point list ref -> unit
(*Saves the coordinates of both players in two .txt files *)
