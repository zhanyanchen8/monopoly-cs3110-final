(** This module represents all the actions that can be 
    performed to change the state of the game. *)

open Player
open Property
open Card
open Gameboard

(** Representation of the current game state. *)
type t

(** This is raised when an attempt is made to load an invalid save *)
exception Invalid_save of string

(** [stall sec] stalls the running of the program for
    [sec] many seconds *)
val stall : float -> unit

(** [clear ()] clears the screen *)
val clear : 'a -> unit

(** [press_enter ()] halts the game until the player presses enter *)
val press_enter : 'a -> unit

(** [buffer_cpu msg] clears the screen, displays [msg], stalls the program
    for 1.5s, and clears the screen again. *)
val buffer_cpu : string -> unit 

(** [buffer msg] clears the screen, displays [msg] for 
    until ENTER is input, and then clears the screen again *)
val buffer : string -> unit

(** [prompt f msg] is the user input [a] iff [f a] evaluates to true. If false,
    msg is printed and [prompt] is called recursively. *)
val prompt : (string -> bool) -> string -> string

(** [init_state board_json players dice board flags filename s] is the state 
    of the game before the first player in [players] makes his/her turn. *)
val init_state : (int * Player.t) list -> Die.t list -> Gameboard.t
  -> bool * bool -> string -> int -> t

(** [get_player n st] is the player number [n] within state [st] *)
val get_player : int -> t -> Player.t

(** [get_players st] is the list of players in st *)
val get_players : t -> Player.t list

(** [get_players_order st] is the list of players and their integer keys in 
    [st] *)
val get_players_order : t -> (int * Player.t) list

(** [get_player_key p st] is the integer key corresponding to [p] in [st] *)
val get_player_key : Player.t -> t -> int

(** [get_board st] is the gameboard used by [st]. *)
val get_board : t -> Gameboard.t

(** [get_properties gb sps] is the complete list of spaces 
    specified by game board [gb] but formatted as properties*)
val get_properties : t -> Property.t list

(** [get_properties_bought st] is properties bought in [st]. *)
val get_properties_bought : t -> int 

(** [monopolies_owned st owned] is the list of unique group names 
    corresponding to each of the groups for which each property is in [owned], 
    where [owned] is the list of all properties owned by the current player 
    in [st] *)
val monopolies_owned : t -> Property.t list -> string list

(** [inc_chance_rotation st] is [st] with chance rotation updated *)
val inc_chance_rotation : t -> t

(** [inc_chest_rotation st] is [st] with chest rotation updated *)
val inc_chest_rotation : t -> t

(** [get_chance_cards st] is the list of Chance cards in [st]. *)
val get_chance_cards : t -> Card.t list

(** [get_chest_cards st] is the list of Community Chest cards in [st]. *)
val get_chest_cards : t -> Card.t list

(** [get_dcount st] is the number of consecutive doubles rolled on this turn 
    in [st]. *)
val get_dcount : t -> int

(** [get_dice st] returns the list of the speed die and the list of normal dice 
    used in [st]. *)
val get_dice : t -> Die.t list * Die.t list

(** [toggle_can_end st] is true when the player is 
    allowed to end their turn. *)
val toggle_can_end : t -> t

(** [get_can_end st] is true when the player is allowed to end their turn. *)
val get_can_end : t -> bool

(** [speed_die_enabled st] is a bool to check if
    speed die is allowed to be used. *)
val speed_die_enabled : t -> bool

(** [trading_enabled st] is a bool to check if trading
    is enabled for this game. *)
val trading_enabled : t -> bool

(** [get_filename st] is the filename associated with this play session *)
val get_filename : t -> string

(** [get_turn_count st] is the number representing the turn that the 
    game is currently on *)
val get_turn_count : t -> int

(** [get_house_count st] is the number of houses currently in play *)
val get_house_count : t -> int

(** [get_hotel_count st] is the number of hotels currently in play *)
val get_hotel_count : t -> int

(** [toggle_game_over st] is [st] with the game_over flag toggled *)
val toggle_game_over : t -> t

(** [get_prop_at pos st] is the property located at position [pos] 
    FAILS is there's no property there*)
val get_prop_at : int -> t -> Property.t

(** [step_turn st] is [st] with turn value changed to the number index of 
    the next non-bankrupt player in order.  *)
val step_turn : t -> t 

(** [current_player st] is the player whose turn it is. *)
val current_player : t -> int

(** [next_player st] is the player whose turn it is. *)
val next_player : t -> int

(** [inc_jtc pl st] is [st] with the player at [pl] having 
    their jail turn count incremented by 1 *)
val inc_jtc : int -> t -> t 

(** [reset_jtc pl st] is [st] with the player at [pl] having 
    their jail turn count reset *)
val reset_jtc : int -> t -> t

(** [player_at_space sp] is the list of
    non-bankrupt players currently at space [sp] *)
val players_at_space : t -> Gameboard.space -> Player.t list

(** [worth pl st] is the value of all the assets held by [pl]. 
    In effect, it's the amount of money that the player would get
    from selling everything that they own. *)
val worth : int -> t -> int

(** [game_status pl st] is a representation of the current
    state of player number [pl] in [st]. *)
val player_status : int -> t -> string

(** [game_status st] is a representation of the current
    state [st] of the game. *)
val game_status : t -> string

(** [build_or_sell bld player prop level st] is [st] with player number 
    [player] building  up to (if [bld] is true) or selling down to 
    (if [bld] is false) [level] on [prop]. 
    If [level] = 5, and [bld] is true, then a hotel is built and the number of 
    houses available to the game increases by the number of houses previously 
    on [prop]. 
    Requires: [player] is allowed to build up or sell down to [level] on [prop]
*)
val build_or_sell : bool -> int -> Property.t -> int -> t -> t

(** [prompt_build_sell bld state prop level] is [state] with [prop] built up to 
    [level] if [bld] is true and the current player can build up to [level] on 
    [prop] in [state], [state] with [prop] sold down to [level] if [bld] is not 
    true, and [state] otherwise. *)
val prompt_build_sell : bool -> Property.t -> int -> t -> t

(** [prompt_build_input bld st] prompts the current player for a property [p] 
    on which to build (if [bld] is true) or sell (if [bld] is false) a level of 
    buildings [l] and then calls [prompt_build] on [p] and [l].  *)
val prompt_build_sell_input : bool -> t -> t

(** [prop_assoc_of f st] is an association list that maps integers
    to the list of properties for which [f prop] evaluates to true
    Note: this association indexes from 1 *)
val prop_assoc_of : (Property.t -> bool) -> t -> (int * Property.t) list

(** [mortgage player prop st] is [st] with [player] having mortgaged [prop]. *)
val mortgage : int -> Property.t -> t -> t

(** [setup_mortgage st] prompts the player to select a property and then
    proceeds to mortgage that property *)
val setup_mortgage : t -> t

(** [unmortgage player prop st] is [st] with 
    player having unmortgaged [prop] *)
val unmortgage : int -> Property.t -> t -> t

(** [setup_unmortgage st] prompts the player to select a property and then
    proceeds to unmortgage that property *)
val setup_unmortgage : t -> t

(** [list_offer props] lists the offers in the list [prop] with a • between 
    each one. Returns "nothing" if [props] is empty *)
val list_offer : Property.t list -> string

(** [get_owned pl st] is the list of all properties owned by player 
    number [pl] in [st]. *)
val get_owned : int -> t -> Property.t list

(** [get_offer pl st] generates the list of properties that the player [pl] 
    wants to put up for trade *)
val get_offer : int -> t -> Property.t list

(** [get_partner] prompts the player to select a trading partner *)
val get_partner : Property.t list -> int -> t -> int

(** [get_compen pl st] generates the list of properties that the current player
    wants to receive from the trading partner [pl] *)
val get_compen : Property.t list -> int -> int -> t -> Property.t list

(** [trade pl1 props1 pl2 props2 st] is [st] with the ownerships of 
    [props1] and [props2] being switched
    Requires: all of [props1] have owner [p1] 
     and all of [props2] have owner [p2] *)
val trade : int -> Property.t list -> int -> Property.t list -> t -> t

(** [setup_trade state] is a creation and processor of 
    trades between players in [state] *)
val setup_trade : t -> t

(** [debted pl owed dbtr st] handles player debt. Should be applied 
     whenever a player is forced to spend money that they don't 
     currently have. The player is prompted to sell their assets 
     (buildings & properties with no buildings) until they have enough 
     money to meet [owed]. If they have no assets and an insufficient           
     amount of money, then the player is declared bankrupt, and all of      
     their worth is given to the debtor player [dbtr] *)
val debted : int -> int -> int -> t -> t

(** [move p dist st] is [st] with player number [p] moved [dist] spaces on the 
    board. If [dist] is positive, the player moves forward. If [dist] is 
    negative, the player moves backward. *)
val move : int -> int -> t -> t

(** [pay_out_jail p st] is a new state where the player of ordinal [p] has paid
    $50 in bail to get out of jail. *)
val pay_out_jail : int -> t -> t

(** [roll st] is [st] with the current player's position changed by a random 
    value, as determined by the dice in [st]. *)
val roll : t -> t

(** [pay_rent player prop st] is [st] where [player] finishes paying 
    rent on [prop] *)
val pay_rent : int -> Property.t -> t -> t

(** [pay_tax player space st] is [st] with [player] having paid the amount of 
    tax corresponding to the [space] it is on. *)
val pay_tax : int -> Gameboard.space -> t -> t

(** [buy player prop cost st] is [st] with owner of [prop] set to [player]
    and [player] with money decreased by [cost]. Normally, [cost] should be 
    the price of [prop]. When called in an auction, [cost] is the winning bid. 
    Requires: [cost] > 0 *)
val buy : int -> Property.t -> int -> t -> t

(** [roll_dice current_name state] is a new game state when current player 
    of [state] rolls the dice. *)
val roll_dice : string -> t -> t

(** [inventory player st] is [st] with list of type [Card.t] containing the
    Chance/Community Chest cards [player] has in hand. *)
val inventory : int -> t -> Card.t list

(** [use player card st] is [st] with [player] having discarded [card]. *)
val use : int -> t -> t

(** [e_winner st] is true iff the current player is the winner of the
    entire game. This happens when the current player is the only one
    who is not bankrupt *)
val e_winner : t -> bool

(** [state_from_file fn st] is this state represented by the contents of
    the file by the name [fn] *)
val state_from_file : string -> t

(** [state_to_file st] is the state [st] saved to a file by the name
    specified by the filename in the respective field [st] *)
val state_to_file : t -> unit

(** [save_turn st] saves the current turn to the directory
    created for this play session *)
val save_turn : t -> unit

(** [load_turn turn fn] is the state in directory [fn] when it was at
    the beginning of turn [turn] 
    FAILS if [turn] is not a turn that has bagun in the past *)
val load_turn : int -> string -> t