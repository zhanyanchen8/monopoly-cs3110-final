(** The module representing a property on a Monopoly board. *)

open Gameboard

(** The abstraction type representing a property of the game *)
type t

(** Represents the buildings that develop properties *)
type building = House | Hotel

(** [init_property name pos price rents group] is a property. *)
val init_property : 
  string -> int -> int -> int list -> group -> t

(** [property_name prop] is the name of [prop]. *)
val property_name : t -> string

(** [property_name_st prop] is the name of [prop]
    along with an extension for the property's status *)
val property_name_st : t -> string

(** [board_ordinal prop] is the ordinal location of [prop]. *)
val board_ordinal : t -> int

(** [get_price prop] is the price of the property *)
val get_price : t -> int

(** [group prop] is the group [prop] is in. *)
val group : t -> group

(** [rent_type prop] is the type of rent basd on [prop]'s [group]. *)
val rent_type : t -> string

(** [rent_values prop] is sequential value of rents to be paid given
    a certain state in the game. 

    If [group prop] yields a [Color], then rent values are based on the number 
    of houses/hotel on [prop] and the rent values of the color. 

    If [group prop] yields [Railroad], then rent values are based on
    the number of railroads owned by a player.

    If [group prop] yields [Utility], then rent values are percentages
    of how much the user rolls, depending on how many utilities a
    player owns. *)
val rent_values : t -> int list

(** [house_cost prop] is the cost to build a house on [t]. *)
val house_cost : t -> int

(** [get_owner prop] is the oridinal integer of the player that owns [t]. *)
val get_owner : t -> int

(** [set_owner name prop] is property [p] with owner set to 
    the ordinal integer [p] *)
val set_owner : int -> t -> t

(** [buildings prop] is a list of [building] on [prop]. *)
val buildings : t -> building list

(** [set_buildings buildings prop] is [prop] with buildings [buildings] *)
val set_buildings : building list -> t -> t
(* place building message here *)

(** [mortgaged prop] is a bool if [prop] is mortgaged. *)
val mortgaged : t -> bool

(** [set_mortgage bool prop] is a [prop] with mortgage set to [bool]. *)
val set_mortgage : bool -> t -> t

(** [toggle_mortgage prop] is a [prop] with mortgage reset. *)
val toggle_mortgage : t -> t

(** [deed prop] is the string representing this the deed 
    of property [prop] *)
val deed : t -> string

(** [property_from_string s] is a property created from its
    respective string representation *)
val property_from_string : string -> t

(** [property_to_string t] is the string representation
    of property [t] *)
val property_to_string : t -> string
