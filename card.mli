(** representation of a Chance/Community Chest card *)

open Yojson.Basic.Util

(** representation of the four functions card can perform *)
type amount_type = 
  | Integer of int
  | Tuple of int * int 
  | Group of string

(** representation of a subsequent action that can be taken
    depending on what the card description says. *)
type card_type

(** representation of the deck a card is in *)
type card_deck =
  | DNone
  | Chance
  | CommunityChest

(** representation of where a card is located in the game *)
type card_location

(** The abstraction type representing a Chance/Community Chest card *)
type t

(** [init_card n desc d f a] is a new card *)
val init_card : string -> string -> string -> string -> string -> t

(** [get_card_name card] is the name of [c]. *)
val get_card_name : t -> string

(** [get_card_description card] is the full description on [card]*)
val get_card_description : t -> string 

(** [get_card_amount card] is the amount to be 
    added/taken away based on [card] type *)
val get_card_amount : t -> amount_type

(** [get_card_type card] is the [card_type] of [c]. *)
val get_card_type : t -> string

(* [get_card_deck card] is the deck [card] is in represented as a string. *)
val get_card_deck : t -> string

(** [shuffle deck] is a new deck with the cards in [deck] shuffled. *)
val shuffle : t list -> t list

(** [change_location card] is [card] with a new [card_location]. *)
val change_location : t -> t

(** [card_from_string s] is a card created from its
    respective string representation *)
val card_from_string : string -> t

(** [card_to_string t] is the string representation
    of card [t] *)
val card_to_string : t -> string