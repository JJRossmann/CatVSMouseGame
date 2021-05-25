val play_game :
  Types.rectangle -> Types.line -> Types.animal -> Types.animal -> int (*returns the winner of a game between a cat and a mouse, on a playing field delimited by a rectangle and with a goal*)
val finding_fifty : Types.animal -> unit (*Prints a list of mouses that play against a cat, with their parameters and winrates*)
val showdown :
  Types.animal array -> Types.animal array -> int array * int array (*puts an array of cats against an array of mouses and returns 2 arrays, with the number of wins of each animal*)
val selection_best : 'a array -> 'b array -> int -> 'a array (*returns an array containing the indices of the highest numbers in a given array*)
val mutation : Types.animal array -> int -> float -> Types.animal array (*returns an array containing mutated animals of the animals in entry*)
val evolution :
  Types.animal array ->
  Types.animal array ->
  int -> int -> float -> Types.animal array ref * Types.animal array ref (*returns a list of cats and mouses after going through showdown, selection_best and mutation a certain number of times*)
