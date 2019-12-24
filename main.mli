(** This module starts the game and manages the interaction between
    the players and the game state. *)

(** [main ()] is the initial player prompt.
    It either goes to new game, load save, or delete save 
    depending on user input. *)
val main : unit -> unit 