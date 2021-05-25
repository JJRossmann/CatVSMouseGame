val draw_environment : Types.game -> unit
(*Draws the rectange of the game *)

val move_animal : Types.point -> Types.point -> unit
(*Draws a line between two points *)

val draw_list_coos : Types.point list -> unit
(*Draws all the movements givent in the Points list *)

val draw_possibilities : Types.animal -> unit
(*Draws the possible movements of the animal *)

val draw_animal : Types.animal -> unit
(*Draws the animal *)

val draw_state_game : Types.game -> unit
(*Draws the position of all the animals in the game with their possibilities. Does NOT draw the game field *)

val show_game : Types.game -> unit
(*Draws the game with its rectangle field and all its animals with their movement options *)

val show_final_state_game :
  Types.game -> Types.point list -> Types.point list -> unit
(*Draws the final state of the game *)

val press_mouse_to_next_screen : unit -> unit
(*Stops the display of the game when we press the mouse button on the display screen *)
