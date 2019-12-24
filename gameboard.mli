(** The representation of a Monopoly gameboard. *)

(** The style of rent that users may pay to other users 
    upon landing on owned property. *)
type rent

(** A community chest or chance card on the gameboard  *)
type card

(** the type that represents the types of spaces (groups) 
    that exist on the game board*)
type group

(** the type that represents the spaces within the game *)
type space

(** the type representing the gameboard loaded in to the 
    game's state*)
type t

val from_json : string -> Yojson.Basic.t -> t

(** [get_gb_name gb] is the name of the gameboard *)
val get_gb_name : t -> string

(** [size gb] is the toal number of spaces in
    gameboard [gb] *)
val size : t -> int

(** [get_space i gb] is the space located at position [i] 
    on gameboard [gb]. All integers are modded into range.*)
val get_space : int -> t -> space

(** [get_postition sp gb] is the ordinal position of space [sp]
    on gameboard [gb] *)
val get_position : space -> t -> int

(** [jail_pos gb] is the ordinal position of space jail on gameboard [gb] *)
val jail_pos : t -> int

(** [gtj_position gb] is the oridinal position of space "Go to Jail" 
    on gameboard [gb]. *)
val gtj_position : t -> int

(** [space_name sp] is the string representation of the 
    name of space [sp] *)
val space_name : space -> string

(** [space_pos sp] is the string representation of the 
    name of space [sp] *)
val space_pos : space -> int

(** [get_space_price sp] is the price associated with the space [sp] *)
val space_price : space -> int

(** [space_group sp] is the string representation of the group that
    this space is a part of *)
val space_group_name : space -> string

(** [get_spaces gb] is the list of spaces in gameboard [gb] *)
val get_spaces : t -> space list

(** [get_card_name c] is the name of [c]. *)
val get_card_name : card -> string

(** [get_card_description c] is the description of [c]. *)
val get_card_description : card -> string

(** [get_card_deck c] is the deck that [c] is in. *)
val get_card_deck : card -> string

(** [get_card_functionality c] is the function the card represents. *)
val get_card_functionality : card -> string

(** [get_card_amount c] is the change in a player 
    based on a card withdrawal. *)
val get_card_amount : card -> string

(** [get_cards gb] is the list of cards associated with [gb]. *)
val get_cards : t -> card list

(** [get_groups gb] is the list of groups in gameboard [gb] *)
val get_groups : t -> group list

(** [get_group_w_name nm gb] is the group in gameboard [gb] with
    name [nm] *)
val get_group_w_name : string -> t -> group

val get_rents : space -> int list

(** [get_gg_name gg] is the name of gameboard_group [gg] *)
val get_gg_name : group -> string

(** [get_gg_build_cost gg] is the build cost of gameboard_group [gg] *)
val get_gg_build_cost : group -> int

(** [get_gg_buildable gg] is whether of gameboard_group [gg] 
    is buildable or not*)
val get_gg_buildable : group -> bool

(** [get_gg_rent gg] is the rent type of gameboard_group [gg] *)
val get_gg_rent : group -> string

(** [group_from_string s] is a group created from its
    respective string representation *)
val group_from_string : string -> group

(** [group_to_string t] is the string representation
    of group [t] *)
val group_to_string : group -> string