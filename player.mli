(** Representation of a player in Monopoly *)

open Property
open Card

(** The abstract type representing a player of the game. *)
type t

(** [create_player name is_cpu] is a player that is constructed
    based on their inputs upon prompts. *)
val create_player : string -> bool -> t

(** [get_name player] is the name of [player]. *)
val get_name : t -> string

(** [check_cpu player] is a bool checking if [player] is a 
    human or a CPU.
    Returns: true, if [player] is a CPU.
             false, [player] is a human. *)
val check_cpu : t -> bool

(** [get_money player] is an int representing the 
    amount of cash [player] has. *)
val get_money : t -> int

(** [change_money player v] is [player] with amount of money changed by [v]. 
    [v] can be any integer value. 
    If the player's money is projected to be negative,
    then the function returns [player] unchanged *)
val change_money : int -> t -> t

(** [reset_money player] is [player] with money set to $0*)
val reset_money : t -> t

(** [get_position player] is an int representing the
    ordinal value of a player's position on the game board. *)
val get_position : t -> int

(** [set_position pos player] is a player with new position [pos]
    on the game board. 
    Requires: [pos] is nonnegative. *)
val set_position : int -> t -> t

(** [check_jail player] is a bool checking if [player] is in jail. *)
val check_jail : t -> bool

(** [toggle_jail player] is a player with prision status toggled. *)
val toggle_jail : t -> t

(** [get_bankrupt player] is true if [player] is bankrupt and false otherwise. 
    A player is bankrupt if their money is <= 0 and they have no unmortgaged 
    properties.  *)
val get_bankrupt : t -> bool

(** [toggle_bankrupt] is a player with bankruptcy status toggled. *)
val toggle_bankrupt : t -> t

(** [inc_jail_turn_count player] is [player] with their jail turn count 
    incremented by 1 *)
val inc_jail_turn_count : t -> t 

(** [reset_jail_turn_count player] is [player] with their jail turn count 
    reset to 0 *)
val reset_jail_turn_count : t -> t

(** [get_jail_turn_count player] is the jail turn count of player [player]
    i.e. the amount of consecutive turns the player has been in jail *)
val get_jail_turn_count : t -> int

(* (** [get_property player prop_name] is a bool checking if [player] owns 
    a property with name [prop_name]. *)
   val get_property : t -> string -> bool *)

(** [add_to_inventory card player] is [player] with [card] added to their 
    inventory. *)
val add_to_inventory : Card.t -> t -> t

(** [remove_from_inventory card player] is player with [card] removed from
    their inventory *)
val remove_from_inventory : Card.t -> t -> t

(** [get_inventory player] is all of the cards 
    [player] has in their inventory. *)
val get_inventory : t -> Card.t list

(* * [status player] is a string detailing the current status of [player].
   val status : t -> string *)

(** [player_from_string s] is a player created from its
    respective string representation *)
val player_from_string : string -> int * t

(** [player_to_string t] is the string representation
    of player [t] *)
val player_to_string : t -> string
