open Game_graphics

(* Returns the best result the current player can reach. 
 * Returns None if the game is finished in the current state. *)
val best_move: state -> move option * result

(* create a list to use with functory mode *)
val liste_base: state -> move list -> state list