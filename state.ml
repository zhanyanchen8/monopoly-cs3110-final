open Unix
open Player
open Property
open Card
open Gameboard
open Yojson.Basic
open Die

let rng = true

type building = Property.building

type t = {
  board : Gameboard.t;
  dice : Die.t list;
  players : (int * Player.t) list;
  properties : Property.t list;
  chance_cards : Card.t list;
  chest_cards : Card.t list;
  dcount : int;
  flags : bool * bool; (** [flags] is speed die, trade *)
  can_end: bool; 
  turn : int;
  roll_value : int;
  game_over : bool;
  filename : string;
  properties_bought : int;
  turn_count : int;
  seed : int;
  chance_rot : int;
  chest_rot : int;
}  

exception Invalid_save of string

let stall sec = 
  let rec stall' dest = 
    if (Stdlib.compare dest (gettimeofday ()) <= 0)
    then () else stall' dest in 
  stall' ((gettimeofday ()) +. sec)

let clear _ = 
  "clear" |> Sys.command |> ignore

let buffer_cpu msg = 
  clear ();
  print_endline msg;
  stall 1.5;
  clear ()

let press_enter _ = 
  print_endline "press ENTER to continue";
  Stdlib.stdin |> Stdlib.input_char |> ignore 

let buffer msg = 
  clear ();
  print_endline msg;
  press_enter ();
  clear ()

(** [pow a b] is a raised to the b power *)
let pow a b =
  let rec pow' acc a = function 
    | 0 -> 1 
    | 1 -> acc 
    | _ as n -> pow' (acc * a) a (n-1)
  in pow' 1 a (b+1)

(** [cmp_fsts x y] is the comparison function on the first elements 
    of [x] and [y] *)
let cmp_fsts x y = Stdlib.compare (fst x)(fst y)

let rec prompt f msg = 
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> prompt f msg
  | s -> if f s then s
    else begin print_string (": " ^ msg ^ "\n"); prompt f msg end

(** [int_of_string_alph s] is the string represented by string [s]
    returns 0 if string is not an integer representation *)
let int_of_string_alph s = 
  try int_of_string s with Failure _ -> 0

(** [filter_props st p] is the list of properties in [st] except for [p]. *)
let filter_props st p = 
  List.filter (fun x -> board_ordinal x <> board_ordinal p) st.properties

(** [filter_players st pl] is the list of all players in [st] except for pl. *)
let filter_players st pl = List.filter (fun x -> (fst x) <> pl) st.players

(** [get_all_cards gb crds] is the complete list of cards 
    specified by game board *)
let get_all_cards crds = 
  let rec get_cards' acc = function 
    | [] -> acc
    | h :: t -> 
      let c = 
        Card.init_card 
          (get_card_name h)
          (get_card_description h)
          (get_card_deck h)
          (get_card_functionality h)
          (get_card_amount h)
      in get_cards' (c :: acc) t
  in get_cards' [] crds

(** [level_ints prop] is the integer representation of the level of buildings 
    on [prop], with [Hotel] represented by [5] *)
let level_ints prop = 
  match Property.buildings prop with
  | [Hotel] -> 5
  | b -> List.length b

(** [modder x m] is a pair of ints of [x/m] and [x mod m]. *)
let modder x m = 
  let rec modder' x wrap m = 
    if x<0 then modder' (x+m) wrap m
    else if x>=m then modder' (x-m) (wrap+1) m
    else (x,wrap)
  in modder' x 0 m

(** [check_prompt_range min max str] is true if [str] can be represented as an 
    integer in the interval [[min,max]]. *)
let check_prompt_range min max str = 
  match int_of_string_opt str with 
  | Some i when i>=min && i<=max -> true
  | _ -> false

(** [rotate_left n lst] rotates the list [lst] to the left [n] times *)
let rec rotate_left n lst =
  if n=0 then lst 
  else match lst with 
    | [] -> []
    | h :: t -> rotate_left (n-1) (t @ [h])

(* [init_chance_cards gb crds] is a list of Chance cards 
    from [crds] of [gb]. *)
let init_chance_cards gb crds = 
  let all_cards = get_all_cards crds in 
  List.filter (fun x -> 
      (Card.get_card_deck x) = "chance") all_cards

(** [init_chest_cards gb crds] is a list of Community Chest cards 
    from [crds] of [gb]. *)
let init_chest_cards gb crds = 
  let all_cards = get_all_cards crds in 
  List.filter (fun x -> 
      (Card.get_card_deck x) = "chest") all_cards

(** [init_properties gb sps] is a list of properties 
    corresponding to each [sps] of [gb]. *)
let init_properties gb sps = 
  let all_prop = let rec get_properties' acc = function 
      | [] -> acc 
      | h :: t -> 
        let p =
          Property.init_property 
            (space_name h)
            (space_pos h)
            (space_price h)
            (get_rents h)
            (get_group_w_name (space_group_name h) gb)
        in get_properties' (p :: acc) t 
    in (get_properties' [] sps)
  in all_prop

let rec init_state p dice gb radios fn s =
  begin 
    if rng then Random.init s else ()
  end;
  ("mkdir " ^ fn) |> Sys.command |> ignore;
  ("clear") |> Sys.command |> ignore;
  make_state p dice gb radios fn s 

and make_state p dice gb radios fn s =
  {
    board = gb;
    dice = begin dice end;
    players = p;
    properties = gb |> get_spaces |> init_properties gb;
    chance_cards = gb |> get_cards |> init_chance_cards gb |> Card.shuffle;
    chest_cards = gb |> get_cards |> init_chest_cards gb |> Card.shuffle;
    dcount = 0; 
    flags = radios;
    can_end = false;
    turn = 0;
    roll_value = 0;
    game_over = false;
    filename = fn;
    properties_bought = 0;
    turn_count = 1;
    seed = s;
    chance_rot = 0;
    chest_rot = 0;
  }

let get_player n st = 
  let rec get_player' n lst = 
    match lst with
    | [] -> failwith ("player not found: " ^ (string_of_int n))
    | h :: t ->
      if (fst h) = n then (snd h) else get_player' n t
  in get_player' n st.players

(** [transact amt pl st] is the player with their money increased or decreased
    by the integer [amt] *)
let transact amt pl state = 
  let player = get_player pl state in
  let new_pl = change_money amt player in 
  let name = get_name player in 
  let verb =
    if amt>0 then "earns"
    else if amt<0 then "loses"
    else "has" 
  in let amt_string = "$" ^ (amt |> abs |> string_of_int) in
  let money_string = 
    "$" ^ (new_pl |> get_money |> abs |> string_of_int) in 
  print_endline (String.concat " " [name; verb; amt_string]);
  print_endline (name ^ " now has " ^ money_string);
  new_pl

let get_players st =
  List.map snd st.players

let get_players_order st = 
  st.players

let get_player_key pl st = 
  let name = get_name pl in
  let rec player_key' = function
    | [] -> failwith "get_player_key player not found"
    | h :: t -> if h |> snd |> get_name = name then fst h else player_key' t
  in player_key' st.players

let get_board st =
  st.board

let get_properties st =
  st.properties

let get_properties_bought st = 
  st.properties_bought

(** [group_list group props] is the list of all properties in [props] that 
    belong to [group]. *)
let rec group_list acc group = function
  | [] -> acc
  | h :: t -> if Property.group h = group 
    then group_list (h :: acc) group t 
    else group_list acc group t

(** [check_owns_group group_list owned] is true if every property in
    [group] is also in [owned] and false otherwise. *)
let rec check_owns_group group_list owned = 
  match group_list with
  | [] -> true
  | h :: t -> if List.mem h owned 
    then check_owns_group t owned 
    else false

let monopolies_owned st owned = 
  let rec build_groups' st acc = function
    | [] -> acc
    | h :: t ->
      let g = Property.group h in 
      let g_name = Gameboard.get_gg_name g in
      let g_list = group_list [] g (get_properties st) in
      if check_owns_group g_list owned 
      then build_groups' st (g_name :: acc) t
      else build_groups' st acc t
  in List.sort_uniq compare (build_groups' st [] owned)

let inc_chance_rotation st = {
  st with chance_rot = st.chance_rot + 1;
}

let inc_chest_rotation st = {
  st with chest_rot = st.chest_rot + 1;  
}

let get_chance_cards st =
  st.chance_cards

let get_chest_cards st = 
  st.chest_cards

let get_dcount state =
  state.dcount

let get_dice st =
  let rec get_dice' acc = function
    | [] -> acc
    | h :: t -> begin 
        match h with 
        | Normal (_,_) -> get_dice' (fst acc, h :: snd acc) t
        | Speed (_,_) -> get_dice' (h::fst acc, snd acc) t
      end
  in get_dice' ([],[]) (st.dice)

let toggle_can_end st = {
  st with can_end = not st.can_end; 
}

let get_can_end st =
  st.can_end

let speed_die_enabled st =
  fst (st.flags)

let trading_enabled st =
  snd (st.flags)

let get_filename st = 
  st.filename

let get_turn_count st = 
  st.turn_count

(** [get_last_roll st] is the previously rolled value in [st]. *)
let get_last_roll st =
  st.roll_value

(** [get_building_count st] is the number of buildings 
    of type [building] currently in play *)
let get_building_count building st = 
  let rec get_count = function 
    | [] -> 0 
    | h :: t -> 
      if h=building 
      then 1 + get_count t 
      else get_count t 
  in st.properties
     |> List.rev_map buildings
     |> List.fold_left (@) []
     |> get_count

let get_house_count st = 
  get_building_count House st

let get_hotel_count st = 
  get_building_count Hotel st

let toggle_game_over st = 
  { st with game_over = not st.game_over }

let get_prop_at pos st = 
  let rec extract_prop = function
    | [] -> failwith "no property here"
    | h :: t -> 
      if (Property.board_ordinal h = pos) 
      then h else extract_prop t
  in extract_prop st.properties

let rec step_turn state = 
  let next = fst (modder (state.turn + 1) (List.length state.players)) in 
  let next_state = {
    state with turn = next;
               dcount = 0;
               can_end = false;
  } in if get_bankrupt (get_player next state) 
  then step_turn next_state  
  else { next_state with turn_count = next_state.turn_count + 1 }

let current_player st =
  st.turn

let next_player st = 
  let next_state = st |> step_turn in
  next_state.turn

(** [change_jail_turn_count f pl st] is [st] with 
    different number of turns in jail. *)
let change_jail_turn_count f pl st = 
  let player = st |> get_player pl |> f in
  let filt = List.filter (fun x -> (fst x)<>pl) st.players in 
  { st with 
    players = (pl,player) :: filt }

let inc_jtc pl st = 
  change_jail_turn_count Player.inc_jail_turn_count pl st 

let reset_jtc pl st = 
  change_jail_turn_count Player.reset_jail_turn_count pl st 

(** [players_at_space st sp] is a list of players on [sp]. *)
let players_at_space st sp = 
  let sp_pos = get_position sp st.board in
  let rec pas acc = function
    | [] -> acc 
    | (ord,pl) :: t -> 
      if (pl |> get_bankrupt |> not) && sp_pos = (Player.get_position pl)
      then pas (pl :: acc) t
      else pas acc t
  in pas [] st.players

(** [comp_prop_pos x y] compares two properties [x] and [y] by their
    ordinal position on the board *)
let comp_prop_pos x y =
  Stdlib.compare (Property.board_ordinal x) (Property.board_ordinal y) 

(** [star_string n] is the string containing [n] many 
    consecutive Unicode stars *)
let rec star_string num = 
  if num=0 then "" else "B" ^ (star_string (num-1))

(** [spacebar n] is [n] many space characters *)
let spacebar n = 
  let rec spacebar' acc n =
    if n <= 0 then acc else spacebar' (" " ^ acc) (n-1)
  in spacebar' "" n

(** [list_player_names lst] adds asterisk if the player is in jail *)
let rec list_player_names = function 
  | [] -> ""
  | h :: [] ->
    (get_name h) ^ (if check_jail h then "*" else "")
  | h1 :: (h2 :: t2 as t) -> 
    (get_name h1) ^ (if check_jail h1 then "*" else "")
    ^ " • " ^ list_player_names t

(* [string_group_buyable g] is "" if [g] is a group that cannot be 
    owned and a string of [g]'s name otherwise. *)
let string_group_buyable g = 
  match space_group_name g with 
  | "go" | "free" | "chance" | "chest" | "tax" -> ""
  | _ as s -> s

(** [get_lengths acc st lst] is the lengths that will be used to
    format status of the game at [st]. *)
let rec get_lengths acc st = function
  | [] -> acc
  | h :: t -> 
    let prop_opt =
      try Some (get_prop_at (space_pos h) st) with Failure _ -> None in
    let name_len = String.length (space_name h) in 
    let group_len = String.length (string_group_buyable h) in 
    let num_build_len =
      match prop_opt with
      | None -> 0
      | Some prop -> if mortgaged prop then 4 else match_lengths prop in 
    let owner_len =  
      match prop_opt with
      | None -> 0
      | Some prop ->
        try st |> get_player (get_owner prop) |> get_name |> String.length 
        with Failure _ -> 0 in 
    let visit_len =  
      h |> players_at_space st |> list_player_names |> String.length in
    form_lengths name_len group_len num_build_len owner_len visit_len acc st t

and match_lengths prop = 
  match Property.buildings prop with 
  | [Hotel] -> 1
  | _ as lst ->
    lst
    |> List.length
    |> star_string
    |> String.length

and form_lengths name_length group_length num_buildings_length 
    owner_length visiting_length acc st t =
  let new_name_length = if name_length > List.nth acc 0 
    then name_length else List.nth acc 0 in
  let new_group_length = if group_length > List.nth acc 1 
    then group_length else List.nth acc 1 in
  let new_num_buildings_length = if num_buildings_length > List.nth acc 2 
    then num_buildings_length else List.nth acc 2 in
  let new_owner_length = if owner_length > List.nth acc 3 
    then owner_length else List.nth acc 3 in
  let new_visiting_length = if visiting_length > List.nth acc 4 
    then visiting_length else List.nth acc 4 in
  get_lengths [new_name_length; new_group_length; new_num_buildings_length;
               new_owner_length; new_visiting_length] st t

(* [choose_greater_less x y] is x if x > y, y otherwise. *)
let choose_greater_less x y = 
  if x > y then x else y

(** [get_plengths acc p lst] is the lengths that will be used to
    format status of [p]. *)
let rec get_plengths acc p = function 
  | [] -> acc 
  | h :: t -> if Property.get_owner h = p 
    then let name_len = h |> property_name |> String.length in 
      let group_len = h |> group |> get_gg_name |> String.length in 
      let dev_len = if mortgaged h then 6 
        else match buildings h with
          | [] -> 0
          | [House] -> 3
          | [House;House] -> 4
          | [House;House;House] -> 5
          | [House;House;House;House] -> 6
          | [Hotel] -> 7
          | _ -> failwith "invalid building configuration" in 
      let (old_name_len, old_group_len, old_dev_len) = 
        (List.nth acc 0, List.nth acc 1, List.nth acc 2) in
      let new_name_len = choose_greater_less name_len old_name_len in 
      let new_group_len = choose_greater_less group_len old_group_len in
      let new_dev_len = choose_greater_less dev_len old_dev_len in
      get_plengths [new_name_len; new_group_len; new_dev_len] p t
    else get_plengths acc p t

(** [format_list lengths strings] is the list of strings fitted to
    the corresponding max length the string can have.
    Prints the string list in reverse order
    Requires: [lengths] and [strings] have the same length *)
let rec format_list acc lengths strings =
  match lengths, strings with
  | [], [] -> acc
  | h1::t1, h2::t2 ->
    let num_spaces = 2 + h1 - (String.length h2) in
    format_list (h2 ^ spacebar num_spaces ^ acc) t1 t2
  | _, _ -> failwith "status print mismatch"

(** [property_string lengths prop] is the string 
    representation of property [prop] *)
let property_string lengths prop = 
  let name = prop |> property_name  in 
  let grp = prop |> group |> get_gg_name in 
  let dev = if mortgaged prop then "(MRTG)"
    else match buildings prop with
      | [] -> ""
      | [House] -> "(B)"
      | [House;House] -> "(BB)"
      | [House;House;House] -> "(BBB)"
      | [House;House;House;House] -> "(BBBB)"
      | [Hotel] -> "(HOTEL)"
      | _ -> failwith "invalid building configuration"
  in format_list "" (List.rev lengths) [dev; grp; name]

(** [properties_string p ps] is the string representing the current player 
    [p]'s owned properties within list [ps] *)
let properties_string (p:int) ps = 
  let lengths = get_plengths [0;0;0] p ps in 
  let rec get_string acc p = function
    | [] -> acc 
    | h :: t -> 
      if Property.get_owner h = p
      then get_string (acc ^ " • " ^ property_string lengths h ^ "\n") p t
      else get_string acc p t 
  in get_string "" p (ps |> List.sort comp_prop_pos)

(** [inventory_string p st] is the inventory of [p] as a string. *)
let inventory_string p st = 
  let cards = get_inventory p in 
  if cards = [] then ""
  else 
    let card = List.hd cards in
    " • " ^ Card.get_card_description card ^ " ("
    ^ Card.get_card_deck card ^ ")" ^ "\n"

(** [prop_worth prop] is the projected amount of money received
    after selling the property's buildings as well as mortgaging it *)
let prop_worth prop st = 
  let rec build_worth bc = function 
    | [] -> 0
    | House :: t -> bc + build_worth bc t 
    | Hotel :: t -> 5 * bc + build_worth bc t in 
  ((get_price prop) / 2) + 
  build_worth ((prop |> group |> get_gg_build_cost) / 2) (prop |> buildings)

let worth pl st = 
  let rec get_worth pl = function 
    | [] -> 0
    | h :: t -> 
      if Property.get_owner h = pl && (h |> mortgaged |> not)
      then (prop_worth h st) + get_worth pl t
      else get_worth pl t in 
  get_worth pl st.properties

let player_status pl st = 
  let curr = get_player pl st in 
  let name = "NAME:\t" ^ get_name curr ^ "\n" in 
  let loc = "LOC:\t" 
            ^ (get_space (curr |> Player.get_position) st.board 
               |> space_name) ^ "\n" in
  let money = get_money curr in 
  let assets = worth st.turn st in 
  let money_string = "FUNDS:\t" ^ "$" ^ string_of_int money ^ "\n" in 
  let assets_string = "WORTH:\t" ^ "$" ^ string_of_int assets ^ "\n" in
  let total_string = 
    if assets=0 then ""
    else "TOTAL:\t" ^ "$" ^ string_of_int (money + assets) ^ "\n" in
  let jail_status = "JAIL?\t" ^ 
                    if check_jail curr then "yes\n" else "no\n" in 
  let prop = "PROPERTIES:\n" ^ properties_string pl  st.properties ^ "\n" in 
  let inventory = "INVENTORY:\n" ^ inventory_string curr st ^ "\n" in
  String.concat ""
    [name; loc; money_string; assets_string;
     total_string; jail_status; prop; inventory]

let rec game_status st = 
  let lengths = get_lengths [4;5;6;5;4] st (get_spaces st.board) in
  format_list "" (List.rev lengths) 
    ["HERE"; "OWNER"; "STATUS"; "GROUP"; "NAME"]
  ^ "\n" ^ get_string_b st lengths (get_spaces st.board)

and get_string_b st lengths = function
  | [] -> ""
  | h :: t -> 
    let prop_opt' =
      try Some (get_prop_at (space_pos h) st) with
      | Failure _ -> None
    in let prop_opt = get_prop prop_opt' in 
    let name = space_name h in 
    let group = string_group_buyable h in 
    let num_buildings = get_status_prop prop_opt in
    let owner = get_owner_prop st prop_opt in
    let visiting = h |> players_at_space st |> list_player_names in
    format_list "" (List.rev lengths) 
      [visiting; owner; num_buildings; group; name]
    ^ "\n" ^ get_string_b st lengths t

and get_prop = function 
  | None -> None 
  | Some prop as v ->
    if Property.get_price prop = ~-1 then None else v

and get_status_prop = function 
  | None -> ""
  | Some prop ->
    if mortgaged prop then "MRTG" else
      print_buildings (Property.buildings prop)

and get_owner_prop st = function 
  | None -> ""
  | Some prop -> try 
      st |> get_player (Property.get_owner prop) |> get_name 
    with Failure _ -> "-----"

and print_buildings  = function 
  | [] -> "-----"
  | [Hotel] -> "HOTEL"
  | _ as lst -> lst |> List.length |> star_string

(* [cons_mult x acc n] is a list with [n] [x]s added to [acc]. *)
let rec cons_mult x acc = function
  | 0 -> acc
  | n -> cons_mult x (x :: acc) (n - 1)

(* [build_of_int l] is number and type of buildings to add given [l]. *)
let build_of_int = function
  | 0 -> []
  | l when l>=1 && l<=4 -> cons_mult House [] l
  | 5 -> [Hotel]
  | _ -> failwith "build_of_int: invalid build level"

(* [match_bld in1 in2 bld] is [in1] if [bld] is true, [in2] otherwise. *)
let match_bld in1 in2 = function 
  | true -> in1 
  | false -> in2

let build_or_sell bld player prop level st = 
  let f_props = filter_props st prop in
  let buildings = build_of_int level in
  let improved = set_buildings buildings prop in
  let f_players = filter_players st player in
  let g_price = prop |> group |> get_gg_build_cost in
  let level_ints prop = 
    match Property.buildings prop with
    | [] -> 0
    | [Hotel] -> 5
    | b -> List.length b
  in let level_change = level - (prop |> level_ints) in
  let spent = match_bld (level_change * g_price) 
      ((level_change * g_price) / 2) bld in
  let player' = transact (~-1 * spent) player st in
  {st with properties = improved :: f_props;
           players = List.sort_uniq cmp_fsts ((player, player')::f_players);
  }

let prompt_build_sell bld prop level state = 
  if level = level_ints prop
  then let bld_str = match_bld "build on " "sell from " bld in 
    print_endline ("You did not " ^ bld_str ^ Property.property_name prop);
    state
  else 
    let lv = match level with
      | 0 -> "Unimproved"
      | 1 -> "1 House"
      | 5 -> "a Hotel"
      | i -> string_of_int i ^ " Houses" in 
    let bld_str' = match_bld "built up" "sold down" bld in 
    let bld_msg = "You have " ^ bld_str' ^ " to " ^ lv
                  ^ " on " ^ Property.property_name prop in
    print_endline bld_msg; 
    build_or_sell bld (current_player state) prop level state

(** [build_sell_levels bld acc props] is [acc], the list of proposed build or 
    sell levels matched with the properties in [props]. If [build] is true, 
    then [build_sell_levels] is build levels, 
    and if not, then it is sell levels.

    If the player owns a hotel on a property, the proposed build level on that 
    property is automatically five.
    If the player has no buildings on a property, the proposed sell level on 
    that property is automatically zero. *)
let rec build_sell_levels bld acc = function
  | [] -> acc
  | h :: t ->
    let name = Property.property_name h in
    let range = match Property.buildings h with
      | [Hotel] -> 5
      | b -> List.length b in 
    if range = 5 && bld
    then hotel_error_msg name bld range h acc t 
    else if range = 0 && not bld
    then print_error_sell name bld range h acc t
    else
      let current_level = get_current_level h in
      let bld_str = get_build_str bld in
      print_level_build name current_level bld_str name;
      let level = get_level range bld in
      let level_num = int_of_string level in 
      build_sell_levels bld ((level_num, h) :: acc) t

and hotel_error_msg name bld range h acc t =
  print_endline ("You have a hotel on " ^ name ^ 
                 " and cannot build further");
  build_sell_levels bld ((range,h) :: acc) t

and print_error_sell name bld range h acc t = 
  print_endline ("You have no buildings on " ^ name ^ " and cannot sell");
  build_sell_levels bld ((range,h) :: acc) t

and print_level_build name current_level bld_str name = 
  print_endline
    (String.concat "\n"
       ["Current level of " ^ name ^ " is " ^ current_level ^ ".";
        "What level would you like to " ^ bld_str ^ " to on " ^ name ^ "?";
        "0. None";
        "1. 1 House";
        "2. 2 Houses";
        "3. 3 Houses";
        "4. 4 Houses";
        "5. Hotel";]);

and get_current_level h = 
  match h |> level_ints with 
  | 0 -> "Unimproved"
  | 1 -> "1 House"
  | 5 -> "Hotel"
  | i -> string_of_int i ^ " Houses"

and get_build_str = function 
  | true -> "build up"
  | false -> "sell down"

and get_level range = function 
  | true -> prompt (fun x -> check_more_hotel x range)
              "You must select a level at or above the current build 
                    level in \ order to build (at most Hotel)"
  | false -> prompt (fun x -> check_less_house x range)
               "You must select a level at or below the current 
                    build level in \ order to sell"

and check_more_hotel x range = 
  check_prompt_range range 5 x

and check_less_house x range = 
  check_prompt_range 0 range x

let get_owned pl st = 
  let props = get_properties st in
  let rec get_owned' acc pl st = function
    | [] -> acc
    | p :: t -> 
      if Property.get_owner p = pl 
      then get_owned' (p :: acc) pl st t
      else get_owned' acc pl st t in 
  get_owned' [] pl st props

(** [sellable st player] determines if a property is sellable or not. *)
let rec sellable st player = 
  let board = st |> get_board in
  let spcs = board |> get_spaces in
  let spcs_buildable = List.filter (fun x -> 
      get_group_w_name (space_group_name x) board |> get_gg_buildable) spcs in
  let spcs_buildable' = List.map (fun x -> 
      let pos = x |> space_pos in 
      find_prop pos st) spcs_buildable in
  let spcs_of_owner = List.filter (fun x -> 
      Property.get_owner x = current_player st) spcs_buildable' in
  List.filter (fun x -> Property.buildings x != []) spcs_of_owner

and find_prop pos st = 
  let props = get_properties st in 
  List.filter (fun x -> Property.board_ordinal x = pos) props |> List.hd

(** [prompt_build_input bld st] prompts the current player for a property [p] 
    on which to build (if [bld] is true) or sell (if [bld] is false) a level of 
    buildings [l] and then calls [prompt_build] on [p] and [l].  *)
let rec prompt_build_sell_input bld st = 
  let curr = st.turn in 
  let pl = get_player curr st in 
  let funds = pl |> Player.get_money |> string_of_int in
  let owned = get_owned curr st in
  let owned' = List.filter 
      (fun x -> x |> Property.group |> Gameboard.get_gg_buildable) owned in
  let groups = monopolies_owned st owned' in 
  let range = List.length groups in
  let bld_str = match_bld "build" "sell" bld in 
  if range = 0 then begin
    print_endline ("cannot " ^ bld_str ^ " - you do not own any monopolies");
    st end
  else prompt_hmn_build_sell st funds bld_str pl groups range bld owned

and cpu_choice st pl groups = 
  let prop = List.hd (sellable st pl) |> group in  
  let rec get_group_num lst g iter = 
    if List.nth_opt lst iter = None 
    then -1
    else if List.nth_opt lst iter = Some g
    then iter 
    else get_group_num lst g iter+1 in 
  get_group_num groups (get_gg_name prop) 0

and print_prompt st funds bld_str =
  print_endline (game_status st);
  print_endline ("FUNDS: $" ^ funds);
  print_endline ("Which color would you like to " ^ bld_str ^ " on?");

and print_info bld_str group_choice' val_str group_price' pl =
  print_endline
    (String.concat "\n" 
       ["You are " ^ bld_str ^ "ing on " ^ group_choice';
        "Buildings " ^ val_str ^ " $" ^ string_of_int group_price' ^ " each";
        "FUNDS: $" ^ (pl |> get_money |> string_of_int)]);

and group_opts_str acc idx = function
  | [] -> acc
  | h :: t -> let acc' = acc ^ string_of_int idx ^ ". " ^ h ^ "\n" in 
    group_opts_str acc' (idx + 1) t

and call_p_build bld state = function
  | [] -> state
  | (l,p) :: t -> 
    let state' = prompt_build_sell bld p l state in
    call_p_build bld state' t

and prompt_hmn_build_sell st funds bld_str pl groups range bld owned = begin 
  print_prompt st funds bld_str; print_endline (group_opts_str "" 1 groups);
  let group_choice = if check_cpu pl then let cpu = cpu_choice st pl groups in
      if cpu > ~-1 then string_of_int cpu else failwith ("can't sell")
    else prompt (fun x -> check_prompt_range 1 range x) "out of range\n" in 
  let group_choice' = 
    match int_of_string group_choice with i -> List.nth groups (i-1) in
  let group_price = st |> get_board |> get_group_w_name group_choice' 
                    |> get_gg_build_cost in
  let group_price' = if bld then group_price else group_price / 2 in 
  let val_str = match_bld "cost" "are worth" bld in
  print_info bld_str group_choice' val_str group_price' pl;
  let props_in_group = List.filter (fun x -> 
      x |> Property.group |> Gameboard.get_gg_name = group_choice') owned in
  let levels = match_bld (build_sell_levels true [] props_in_group)
      (build_sell_levels false [] props_in_group) bld in
  let current_levels = List.fold_left ( + ) 0 
      (props_in_group |> List.map (fun x -> level_ints x)) in
  let new_levels = List.fold_left ( + ) 0 (List.map fst levels) in
  let total_cost = group_price * (new_levels - current_levels) in
  display_output total_cost pl bld group_choice' st levels end

and display_output total_cost pl bld group_choice' st levels = 
  if total_cost > get_money pl && bld
  then begin print_endline "You don't have enough money for that!"; st end
  else let result_str = match_bld 
           ("You spent $" ^ string_of_int total_cost 
            ^ " to build on " ^ group_choice')
           ("You earned $" ^ string_of_int ((~-1 * total_cost)/2)
            ^ " for selling from " ^ group_choice') bld in 
    print_endline result_str;
    call_p_build bld st levels

let prop_assoc_of f st = 
  let rec pao acc idx f = function 
    | [] -> acc
    | h :: t -> 
      if f h
      then pao ((idx,h) :: acc) (idx+1) f t
      else pao acc idx f t in 
  st |> get_properties 
  |> List.sort comp_prop_pos 
  |> pao [] 1 f 
  |> List.sort cmp_fsts

(** [grab_prop priority prop_assoc] takes the first [prop_assoc] of a 
    list that matches the first [priority] and returns that property. 
    Requires: [prop_assoc] != [] *)
let rec grab_prop priority prop_assoc = 
  match priority with 
  | [] -> None
  | h::t -> 
    let props_of_h = List.filter (fun x -> 
        Property.group (snd x) = h) prop_assoc in 
    if props_of_h = [] 
    then grab_prop t prop_assoc 
    else Some (props_of_h |> List.hd |> snd)

(** [list_mrtgble prop_assoc] is the prompt string for selecting a property 
    to mortgage *)
let rec list_mrtgble = function
  | [] -> "" 
  | (k,v) :: t -> 
    string_of_int k ^ ". "
    ^ Property.property_name_st v
    ^ " ($" ^ string_of_int ((v |> get_price) / 2)  ^ ")"
    ^ "\n" ^ list_mrtgble t

(** [list_owned prop_assoc] is the prompt string for selecting a property *)
let rec list_owned = function
  | [] -> "" 
  | (k,v) :: t -> 
    string_of_int k ^ ". "
    ^ Property.property_name_st v
    ^ "\n" ^ list_owned t

(** [list_unmrtgble prop_assoc] is the prompt string for selecting a property 
    to unmortgage *)
let rec list_unmrtgble = function
  | [] -> "" 
  | (k,v) :: t -> 
    string_of_int k ^ ". "
    ^ Property.property_name_st v
    ^ " ($" ^ string_of_int ((v |> get_price |> ( * ) 110) / 200) ^ ")"
    ^ "\n" ^ list_unmrtgble t

(** [priority_organizing st] is a list of groups in the most important order,
    starting from the median position on the board and wrapping around
    the median until space 0 on board in [st] is reached. *)
let rec priority_organizing st = 
  let board = get_board st in 
  let size = Gameboard.size board in 
  let med_loc = size/2 in 
  let g = create_priority [] (0,0) true med_loc board in 
  let g' = List.filter (fun x -> 
      Gameboard.get_gg_buildable x || 
      Gameboard.get_gg_name x = "rail" || 
      Gameboard.get_gg_name x = "utility") g in 
  let str_g' = List.map (fun x -> Gameboard.get_gg_name x) g' in 
  let rail_in = List.mem "rail" str_g' in 
  let utility_in = List.mem "utility" str_g' in 
  let new_g = List.filter (fun x -> 
      not (x="rail"||x="utility")) str_g' in 
  let g'' = if rail_in then new_g@["rail"] else new_g in 
  let g''' = if utility_in then g''@["utility"] else g'' in 
  List.map (fun x -> Gameboard.get_group_w_name x board) g'''

and create_priority acc (l,r) dec med_loc board = 
  if l=0 && r=0 
  then 
    let group = get_group_of_curr_spc med_loc 0 board in
    create_priority [group] (l+1, r+1) dec med_loc board
  else if l < med_loc && r < med_loc 
  then if dec 
    then let group = get_group_of_curr_spc med_loc ~-l board in
      if List.mem group acc
      then create_priority acc (l+1, r) true med_loc board
      else create_priority (acc@[group]) (l+1, r) false med_loc board
    else let group = get_group_of_curr_spc med_loc r board in
      if List.mem group acc 
      then create_priority acc (l,r+1) false med_loc board
      else create_priority (acc@[group]) (l, r+1) true med_loc board
  else fill_in_merge acc l r med_loc board

and fill_in_merge acc l r med_loc board = 
  if l < med_loc then
    let group = get_group_of_curr_spc med_loc ~-l board in
    if List.mem group acc 
    then create_priority (acc) (l+1, r) true med_loc board
    else create_priority (acc@[group]) (l+1, r) true med_loc board
  else if r < med_loc then 
    let group = get_group_of_curr_spc med_loc r board in
    if List.mem group acc 
    then create_priority acc (l,r+1) false med_loc board
    else create_priority (acc@[group]) (l, r+1) true med_loc board
  else acc

and get_group_of_curr_spc med_loc diff board = 
  let g = Gameboard.get_space (med_loc + diff) board
          |> Gameboard.space_group_name in
  Gameboard.get_group_w_name g board

let mortgage player prop st = 
  let owner_ord = Property.get_owner prop in
  if owner_ord = player then begin
    print_string (Property.property_name prop);
    print_endline " is now mortgaged";
    {
      st with 
      properties = begin
        let f_props = filter_props st prop in
        let mortgaged = Property.toggle_mortgage prop in
        mortgaged :: f_props;
      end;
      players = begin
        let filtered_players = filter_players st player in 
        let buy_value = Property.get_price prop in 
        let new_player = transact (buy_value/2) player st in
        List.sort_uniq cmp_fsts ((player, new_player)::filtered_players);
      end;
    } 
  end
  else st

let rec setup_mortgage st = 
  let curr = current_player st in 
  let prop_assoc = prop_assoc_of (fun x -> 
      Property.get_owner x = curr && x |> mortgaged |> not &&
      Property.buildings x = []) st in 
  if check_cpu (get_player curr st) then setup_mortgage_cpu st curr prop_assoc
  else if prop_assoc |> List.length |> (<>) 0 
  then prompt_hmn_mort prop_assoc curr st
  else begin 
    print_endline "cannot mortgage - you own no unmortgaged properties"; st
  end

and setup_mortgage_cpu st curr prop_assoc= 
  let priority = List.rev (priority_organizing st) in 
  let prop = grab_prop priority prop_assoc in 
  match prop with 
  | None -> 
    print_endline "cannot mortgage - you own no unmortgaged properties"; st
  | Some p -> mortgage curr p st

and prompt_hmn_mort prop_assoc curr st = begin
  print_endline "select a property to mortgage (0 to cancel)"; 
  print_string (list_mrtgble prop_assoc);
  let choice = prompt (fun x ->
      try (int_of_string x) >= 0 && 
          (int_of_string x) <= (List.length prop_assoc)
      with Failure _ -> false) "invalid\n" |> int_of_string in 
  if choice = 0 then st 
  else let prop = List.assoc choice prop_assoc in
    if Property.mortgaged prop then begin 
      print_endline (Property.property_name_st prop^" is already mortgaged");
      st end
    else match Property.buildings prop with
      | [] -> mortgage curr prop st
      | _ -> begin 
          print_endline
            (Property.property_name prop 
             ^ " cannot be mortgaged because it has buildings on it"); st
        end 
end

let rec unmortgage player prop st =
  let owner_ord = Property.get_owner prop in
  let f_props = filter_props st prop in
  let mortgaged = Property.toggle_mortgage prop in
  let curr_player = get_player player st in
  let filtered_players = filter_players st player in 
  let buy_value = Property.get_price prop in 
  let amt = (buy_value/2)*110/100 in 
  process_unmortgage amt curr_player prop owner_ord player mortgaged f_props 
    filtered_players st

and process_unmortgage amt curr_player prop owner_ord player mortgaged f_props 
    filtered_players st = 
  if amt > get_money curr_player 
  then begin 
    print_endline ("you cannot afford to unmortgage " ^ property_name prop); st
  end
  else if owner_ord = player then begin
    print_endline (property_name prop ^ " is now unmortgaged");
    {st with properties = mortgaged :: f_props;
             players = let new_player = transact (-1 * amt) player st in
             List.sort_uniq cmp_fsts ((player, new_player)::filtered_players);
    } 
  end
  else st

let rec setup_unmortgage st = 
  let curr = current_player st in 
  let prop_assoc = prop_assoc_of (fun x -> 
      Property.get_owner x = curr && mortgaged x) st in
  if check_cpu (get_player curr st) 
  then setup_unmortgage_cpu st curr prop_assoc
  else if prop_assoc |> List.length |> (<>) 0 
  then begin get_unmort_prop_st st curr prop_assoc end
  else begin 
    print_endline "cannot unmortgage - you own no mortgaged properties"; st
  end

and setup_unmortgage_cpu st curr prop_assoc = 
  let priority = priority_organizing st in 
  let prop = grab_prop priority prop_assoc in 
  match prop with 
  | None -> 
    print_endline "cannot unmortgage - you own no mortgaged properties";
    st
  | Some p -> unmortgage curr p st

and get_unmort_prop_st st curr prop_assoc = 
  print_endline ("FUNDS: $" ^ 
                 (st |> get_player curr |> get_money |> string_of_int));
  print_endline "select a property to unmortgage (0 to cancel)";
  print_string (list_unmrtgble prop_assoc);
  let choice = prompt (check_prompt_range 0 (List.length prop_assoc)) 
      "invalid\n" |> int_of_string in 
  if choice = 0 then st 
  else let prop = List.assoc choice prop_assoc in
    if prop |> Property.mortgaged |> not 
    then begin 
      print_string (Property.property_name_st prop);
      print_endline " is currently not mortgaged";
      st
    end
    else unmortgage curr prop st

(** [comp_prop_pos x y] compares two properties [x] and [y] by their
    ordinal position on the board *)
let comp_prop_pos x y =
  Stdlib.compare (Property.board_ordinal x) (Property.board_ordinal y) 

let rec get_list = function 
  | [] -> "" 
  | h :: [] -> Property.property_name_st h
  | h1 :: (h2 :: t2 as t) ->
    Property.property_name_st h1 ^ " • " ^ get_list t

let list_offer = function 
  | [] -> "nothing"
  | h1 :: t1 as t -> get_list t

(** [same_owner owner props] is true iff all of 
    [props] has the same owner *)
let rec same_owner owner = function 
  | [] -> true 
  | h :: t -> (h |> Property.get_owner) = owner && same_owner owner t 

(** [update_props news olds] is the list of properties
    [olds] updated with the properties in [news] *)
let update_props news olds = 
  let new_ords = List.rev_map (fun x -> board_ordinal x) news in 
  let filt = List.filter 
      (fun x -> new_ords |> List.mem (board_ordinal x) |> not) olds in 
  List.rev_append news filt

(** [assign_owner owner props] is the list of 
    properties [properties] with owner [owner] *)
let assign_owner owner props = 
  let rec assign_owner' acc owner = function 
    | [] -> acc 
    | h :: t -> 
      assign_owner' ((h |> Property.set_owner owner) :: acc) owner t
  in 
  assign_owner' [] owner props

let rec get_offer pl st = 
  if check_cpu (get_player pl st)
  then let off = get_offer_cpu pl st in 
    if off = [] then [] else [List.hd (get_offer_cpu pl st)]
  else 
    let prop_assoc = prop_assoc_of (fun x -> Property.get_owner x = pl) st in 
    offer' [] prop_assoc st

and offer' acc prop_assoc st =
  clear ();
  print_endline (game_status st);
  print_endline ("select what you would like to put up for trade "^
                 "(0 for done)");
  print_endline ("current offer: " ^ list_offer acc);
  print_string (list_owned prop_assoc);
  let choice = prompt (fun x -> 
      x="q" || x="e" || x="d"
      || check_prompt_range 0 (List.length prop_assoc) x) "invalid"
               |> int_of_string in
  if choice=0
  then acc
  else begin
    let prop = List.assoc choice prop_assoc in 
    if List.mem prop acc
    then offer' (List.filter (fun x -> x<>prop) acc) prop_assoc st
    else offer' (prop :: acc) prop_assoc  st
  end 

and get_offer_cpu pl st = 
  let props = get_properties st in 
  let owned_props = get_owned pl st in
  match owned_props with 
  | [] -> [] 
  | _ -> get_desired props owned_props st  

and get_desired props owned_props st =
  let groups' = priority_organizing st in 
  let rec parse_groups acc = function
    | [] -> acc
    | h::t -> 
      let all_props_of_group = List.filter (fun x -> 
          Property.group x = h) props in
      let d' = diff_lst [] all_props_of_group owned_props in     
      if d' = [] then [] else
        let d = [List.hd d'] in 
        parse_groups (acc@d) t
  in parse_groups [] (groups')

and diff_lst acc all_props owned_props = 
  match owned_props with 
  | [] -> acc 
  | h::t -> 
    if List.mem h all_props then diff_lst acc all_props t
    else diff_lst (h::acc) all_props t

(** [player_assoc f st] is an association list that maps integers 
    to the players in the game 
    Node: this association indexes from 1 *)
let player_assoc f st = 
  let rec p_assoc acc idx = function
    | [] -> acc
    | pair :: t ->
      if f pair
      then p_assoc ((idx,pair) :: acc) (idx+1) t 
      else p_assoc acc idx t in 
  p_assoc [] 1 (get_players_order st) |> List.sort cmp_fsts

(** [list_partners partner_assoc] is the prompt string for selecting a
    trading partner *)
let rec list_partners = function
  | [] -> "" 
  | (k,(o,v)) :: t -> 
    string_of_int k ^ ". " ^ Player.get_name v ^ "\n" ^ list_partners t

let get_partner comp pl st = 
  clear ();
  if check_cpu (get_player pl st)
  then if comp = [] then -1 else Property.get_owner (List.hd comp)
  else let partner_assoc = player_assoc (fun x ->
      fst x <> current_player st 
      && snd x |> Player.get_bankrupt |> not) st in
    print_endline (game_status st);
    print_endline "Select a trading partner";
    print_string (list_partners partner_assoc);
    let choice = 
      prompt (check_prompt_range 1 (List.length partner_assoc)) "invalid\n"
      |> int_of_string 
    in 
    partner_assoc |> List.assoc choice |> fst

let rec get_compen offer inst pl st = 
  if offer = [] then [] else
  if check_cpu (get_player inst st)
  then get_compen_cpu offer inst st
  else 
    let prop_assoc = prop_assoc_of (fun x -> Property.get_owner x = pl-1) st in 
    compen' [] offer prop_assoc st

and get_compen_cpu offer inst st =  
  let want_from = List.filter (fun x -> 
      Gameboard.get_gg_buildable (Property.group x) && 
      Property.get_owner x <> -1 && 
      Property.get_owner x <> inst) (get_properties st) in
  let rec get_first_nth acc n = function 
    | [] -> acc 
    | h::t ->
      if n > 0 then get_first_nth (h::acc) (n-1) t
      else acc in 
  get_first_nth [] (List.length offer) want_from

and compen' acc offer prop_assoc st = 
  clear ();
  print_endline (game_status st);
  print_endline "select what you would like to receive? (0 for done)";
  print_endline ("current offer: " ^ list_offer offer);
  print_endline ("current yield: " ^ list_offer acc);
  print_string (list_owned prop_assoc);
  let choice = 
    prompt (fun x -> 
        x="q" || x="e" || x="d" 
        || check_prompt_range 0 (List.length prop_assoc) x) "invalid\n"
    |> int_of_string_alph in
  if choice=0
  then acc
  else begin
    let prop = List.assoc choice prop_assoc in 
    if List.mem prop acc
    then compen' (List.filter (fun x -> x<>prop) acc) offer prop_assoc st
    else compen' (prop :: acc) offer prop_assoc st
  end

let trade pl1 props1 pl2 props2 st = 
  let news1 = assign_owner pl2 props1 in 
  let news2 = assign_owner pl1 props2 in 
  let news = List.rev_append news1 news2 in 
  print_endline (game_status st);
  { st with 
    properties = update_props news st.properties }

let rec setup_trade st = 
  let curr = current_player st in 
  let name = st |> get_player curr |> Player.get_name in
  let cpu = check_cpu (get_player curr st) in clear ();
  let offer = get_offer curr st in 
  let compen' = if cpu 
    then get_compen offer curr ~-2 st 
    else [] in
  let partner = if cpu
    then get_partner compen' curr st
    else get_partner offer curr st in 
  if partner = -1 
  then begin buffer_cpu "Suitable trade cannot be found"; st end
  else let compen = 
         if compen' = [] 
         then get_compen offer curr (partner+1) st
         else compen' in
    clear ();
    trade_accept_deny curr name offer partner compen st

and trade_accept_deny curr name offer partner compen st = 
  let partner_name = st |> get_player partner |> Player.get_name in
  let approved = begin
    if Player.check_cpu (get_player partner st) then false
    else let poss = if partner_name.[String.length partner_name - 1] = 's' 
           then "'" else "'s" in buffer (partner_name ^ poss ^ " choice:");
      print_endline (game_status st); 
      print_endline (partner_name ^ poss ^ " choice:");
      print_endline "do you approve of this trade? (y/n)";
      print_endline (name ^ " offers: " ^ list_offer offer);
      print_endline ("in exchange for: " ^ list_offer compen);
      match prompt (fun x -> x="y" || x="n") "invalid\n" with 
      | "y" -> true
      | "n" -> false
      | _ -> failwith "never happens"
  end in 
  if approved then begin 
    print_endline "trade approved";
    trade curr offer partner compen st
  end
  else begin print_endline "trade denied"; st end

(** [change_ownerships pl dbtr props] is [prop] but if owned by 
    [pl] is instead owned by [dbtr]
    Additionally, it removes the buildings on the property and if 
    the property is not going to the bank, it is mortgaged *)
let change_ownership pl dbtr prop = 
  if get_owner prop = pl && dbtr = ~-1
  then prop |> set_owner dbtr |> set_buildings [] |> set_mortgage false
  else if get_owner prop = pl
  then prop |> set_owner dbtr |> set_buildings [] |> set_mortgage true
  else prop

(** [accrue pl owed dbtr st] prompts the player to accrue the money that 
    they owe *)
let rec accrue pl owed dbtr st = 
  let player = get_player pl st in 
  let debtor =
    if dbtr = ~-1
    then create_player "the bank" true
    else get_player dbtr st in 
  let player_name = get_name player in 
  let poss = if player_name.[String.length player_name - 1] = 's' 
    then "'" else "'s" in
  let debtor_name = get_name debtor in
  let owed_string = string_of_int owed in
  print_endline ("DEBT:\t$" ^ owed_string);
  print_endline ("FUNDS:\t$" ^ (player |> get_money |> string_of_int));
  if get_money player >= owed
  then debt_to_be_paid player_name poss player owed debtor debtor_name 
      owed_string pl dbtr st 
  else prompt_accrue_input player pl owed dbtr st

and debt_to_be_paid player_name poss player owed debtor debtor_name owed_string
    pl dbtr st = begin
  print_endline (player_name ^ poss ^ " debt will now be paid");
  let player' = player |> change_money (~-1 * owed) in 
  let debtor' = debtor |> change_money owed in 
  print_endline (player_name ^ " pays " ^ debtor_name ^ " $" ^ owed_string);
  print_endline (player_name ^ " now has $" ^ 
                 (player' |> get_money |> string_of_int));
  let filt =
    st.players |> List.filter (fun x -> fst x <> pl && fst x <> dbtr) in 
  if dbtr = ~-1
  then begin
    {st with players = (pl,player') :: filt;}
  end
  else begin
    print_endline (debtor_name ^ " now has $" ^ 
                   (debtor' |> get_money |> string_of_int));    
    { st with players = (pl,player') :: (dbtr,debtor') :: filt;}
  end
end

and prompt_accrue_input player pl owed dbtr st = begin 
  print_endline "1. mortgage\n2. sell";
  if check_cpu player
  then 
    let to_sell = sellable st player in
    if to_sell = [] 
    then st |> setup_mortgage |> accrue pl owed dbtr 
    else st |> prompt_build_sell_input false |> accrue pl owed dbtr
  else let choice = prompt (fun x -> x="1" || x="2") "invalid\n" in 
    match choice with 
    | "1" -> st |> setup_mortgage |> accrue pl owed dbtr
    | "2" -> st |> prompt_build_sell_input false |> accrue pl owed dbtr
    | _ -> failwith "never happens"
end

let rec debted pl owed dbtr st = 
  let player = st |> get_player pl in
  let assets = (get_money player) + (worth pl st) in
  let player_name = get_name player in
  let poss = if player_name.[String.length player_name - 1] = 's' 
    then "'" else "'s" in
  let dbtr_name = 
    if dbtr = -1 then "the bank"
    else st |> get_player dbtr |> get_name
  in
  print_endline (player_name ^ " owes $" ^ 
                 string_of_int owed ^ " to " ^ dbtr_name);
  if assets >= owed 
  then accrue pl owed dbtr st
  else process_debt dbtr dbtr_name pl player_name poss assets st 

and process_debt dbtr dbtr_name pl player_name poss assets st = begin
  let filt = if dbtr = -1
    then st.players |> List.filter (fun x -> fst x <> pl)
    else st.players |> List.filter (fun x -> fst x <> pl && fst x <> dbtr) in 
  let new_pl =
    st |> get_player pl |> Player.toggle_bankrupt |> Player.reset_money in 
  let new_dbtr = if dbtr = -1
    then create_player "the bank" true
    else st |> get_player dbtr |> Player.change_money assets in 
  print_endline (get_name new_pl ^ " is bankrupt!");
  print_endline (player_name ^ poss ^ " assets now belong to " ^ dbtr_name);
  { st with players = if dbtr = -1 then (pl,new_pl) :: filt 
              else (pl,new_pl) :: (dbtr,new_dbtr) :: filt ;
            properties =
              st.properties |> List.rev_map (change_ownership pl dbtr);
  } |> (fun x -> if st.turn=pl then step_turn x else x)
end

let rec move p dist st =
  let player = get_player p st in
  let current_loc = Player.get_position player in 
  let board = get_board st in
  let modded = modder (current_loc + dist) (Gameboard.size board) in
  let dest = fst modded in
  let num_cross_go = snd modded in
  let new_player' = move_to_pos dest player board in
  let new_player = pass_go_add_200 new_player' num_cross_go in
  let f_players = filter_players st p in 
  let mod_players = (p,new_player) :: f_players in
  let players' = List.sort_uniq cmp_fsts mod_players in
  { st with players = players' }

and move_to_pos dest player board = 
  if dest = gtj_position board
  then begin
    print_endline ("landed on go to jail\nstraight to jail for you!");
    player |> set_position (jail_pos board) |> toggle_jail
  end
  else begin 
    let dest_name = get_space dest board |> space_name in 
    let dest_group = get_space dest board |> space_group_name in
    print_string ("landed on " ^ dest_name);
    if dest = jail_pos board 
    then print_endline " - Just visiting."
    else 
      begin match dest_group with 
        | "go" | "free" | "chance" | "chest" | "tax" -> print_char '\n'
        | _ as s -> print_endline (" (" ^ s ^ ")")
      end;
    set_position dest player 
  end 

and pass_go_add_200 new_player' num_cross_go = 
  if not (check_jail new_player') 
  then begin 
    let pass_go_amt = 200*num_cross_go in 
    if pass_go_amt <= 0
    then ()
    else print_endline
        ("Collect " ^ "$" ^ string_of_int pass_go_amt 
         ^ " for passing GO");
    change_money pass_go_amt new_player' 
  end
  else new_player'

let pay_out_jail p st =
  let f_players = filter_players st p in 
  let player = get_player p st in
  if get_money player >= 50 then begin
    print_endline "Pay $50"; 
    let this_player = transact ~-50 p st in  
    let out_player = toggle_jail this_player in 
    let players' = List.sort_uniq cmp_fsts ((p, out_player)::f_players) in 
    { st with players = players'} |> reset_jtc p
  end
  else if Player.get_jail_turn_count player >= 3
  then 
    let st' = debted p 50 ~-1 st in 
    let filt = filter_players st p in 
    let pl = st' |> get_player p |> toggle_jail in 
    { st' with players = (p,pl) :: filt }
  else begin
    print_endline "You can't afford that option";
    let players' = List.sort_uniq cmp_fsts ((p, player) :: f_players) in 
    { st with players = players'}
  end

(** [roll_helper dice acc] is the list of values rolled for each of the dice 
    in [dice] *)
let rec roll_helper dice acc = 
  match dice with 
  | [] -> acc
  | h :: t -> roll_helper t ((roll_die h):: acc)

(** [str_s_roll in] is [in] as a string. *)
let str_s_roll = function
  | Bus -> "Bus"
  | Man -> "Monopoly Man"
  | SInt i -> string_of_int i

(** [print_rolls lst] prints values in [lst] *)
let rec print_rolls = function
  | [] -> ()
  | h :: t -> 
    print_endline (string_of_int h); 
    print_rolls t

(** [check_doubles rolls] is true if each value of [rolls] is equal to all 
    other values, and false otherwise.
    [rolls] must have at least 2 values for [check_doubles] to be true *)
let check_doubles = function
  | [] -> false
  | [_] -> false
  | h::t2 as t -> 
    let rec all_equal = function
      | [] -> false
      | [_] -> true
      | h1 :: (h2 :: t2 as t) -> h1=h2 && all_equal t 
    in 
    all_equal t

(** [check_triples r_vals s_val] is true if [s_val] is equal to all values 
    of [r_vals].
    Requires: all values of [r_vals] are equal. *)
let check_triples r_vals s_val = 
  match r_vals with 
  | h :: _ -> h = s_val
  | _ -> false

(** [check_properties owner p prop st] TODO: document *)
let check_properties owner p prop st = 
  let str_group_name = Gameboard.get_gg_name (Property.group prop) in
  let props_same_group = List.filter (fun x -> 
      Gameboard.get_gg_name (Property.group x) = str_group_name) 
      (get_properties st) in 
  let owned = List.filter (fun x -> 
      Property.get_owner x <> Property.get_owner prop) 
      props_same_group in 
  owned = []

(** [check_specials p acc] is how many railroads or utilities [p] owns. *)
let rec check_specials p acc = function 
  | [] -> acc
  | h::t ->
    if Property.get_owner h = p
    then check_specials p (acc+1) t 
    else check_specials p acc t

let rec pay_rent p prop st = 
  let owner = Property.get_owner prop in assert (owner <> ~-1);
  let current_player = get_player p st in
  let f2_plys = filter_players st p |> List.filter (fun x -> fst x <> owner) in
  let rent_values = Property.rent_values prop in 
  if not (check_properties owner p prop st) 
  then process_regs owner p current_player f2_plys rent_values prop st   
  else process_mon_no_house owner p current_player f2_plys rent_values prop st

and process_regs owner p current_player f2_plys rent_values prop st = 
  match Property.rent_type prop with 
  | "direct" -> pay_direct_rent prop 1 rent_values current_player 
                  f2_plys p owner st
  | "variable" -> pay_railroad_rent prop p current_player 
                    f2_plys owner st
  | "percentage" -> pay_utility_rent prop owner p current_player 
                      f2_plys st
  | _ -> failwith "only direct rents implemented1"

and process_mon_no_house owner p current_player f2_plys rent_values prop st = 
  match Property.rent_type prop with 
  | "direct" -> pay_direct_rent prop 2 rent_values current_player 
                  f2_plys p owner st
  | "variable" ->  pay_railroad_rent prop p current_player 
                     f2_plys owner st
  | "percentage" -> pay_utility_rent prop owner p current_player f2_plys st
  | _ -> failwith "only direct rents implemented2" 

and pay_direct_rent prop factor rent_values current_player 
    f2_players p owner st = 
  let type_buildings = Property.buildings prop in
  let pay_this = match type_buildings with
    | [] -> begin factor * List.hd rent_values end
    | h :: _ -> 
      if h = Hotel then rent_values |> List.rev |> List.hd
      else List.nth rent_values (List.length type_buildings) in 
  if pay_this > Player.get_money current_player
  then debted p pay_this owner st 
  else make_transaction pay_this p owner f2_players st

and pay_railroad_rent prop p current_player f2_players owner st = 
  let props = get_properties st in
  let railroads = 
    List.filter (fun x -> 
        Property.group x = Property.group prop) props in
  let num_rails_owned = (check_specials owner 0 railroads) - 1 in
  let pay_this = List.nth (Property.rent_values prop) num_rails_owned in 
  if pay_this > Player.get_money current_player
  then debted p pay_this owner st
  else make_transaction pay_this p owner f2_players st

and pay_utility_rent prop owner p current_player f2_players st =
  let props = get_properties st in
  let utilities = List.filter (fun x -> 
      Property.group x = Property.group prop) props in
  let num_utilities_owned = check_specials owner 0 utilities - 1 in
  let pay_this = (get_last_roll st) *
                 (List.nth (Property.rent_values prop) num_utilities_owned) in 
  if pay_this > Player.get_money current_player
  then debted p pay_this owner st
  else make_transaction pay_this p owner f2_players st

and make_transaction pay_this p owner f2_players st = 
  let less_money = transact (-1 * pay_this) p st in
  let more_money = transact pay_this owner st in 
  { st with players = List.sort_uniq cmp_fsts 
                ((p, less_money) :: (owner, more_money) :: f2_players) }

let buy player prop cost st = 
  let group_name = get_gg_name (Property.group prop) in
  if group_name = "free" || group_name = "chest" || group_name = "chance"
  then st 
  else
    let f_prop = filter_props st prop in
    let new_prop = Property.set_owner player prop in
    let f_players = filter_players st player in 
    let this_player = transact (~-1 * cost) player st in
    print_string (Player.get_name this_player);
    print_string " now owns ";
    print_endline (Property.property_name new_prop);
    { st with properties =  new_prop :: f_prop;
              players = 
                List.sort_uniq cmp_fsts ((player, this_player)::f_players);
              properties_bought = st.properties_bought + 1;
    }

(** [list_bidders players] is a string of the player names in [players]
    separated by • *)
let rec list_bidders = function
  | [] -> "none" 
  | h :: [] -> Player.get_name h
  | h1 :: (h2 :: t2 as t) ->
    Player.get_name h1 ^ " • " ^ list_bidders t

(** [auction st prop bid players] is [st] with the highest bidder in [players] 
    having bought [prop] for [bid].
    Requires: [bid] > 0.
    Requires: [players] <> [].
    Each new bid must be greater than [bid]. 
    If a player enters a bid of 0, they are removed from the auction. 
    When only one player remains in the auction, the purchase is made. *)
let rec auction st prop bid players = clear ();
  match players with 
  | [] -> st
  | h :: [] -> let name = h |> Player.get_name in 
    let funds = h |> Player.get_money |> string_of_int in
    let poss = if name.[String.length name - 1] = 's' then "'" else "'s" in
    let cost = max 1 bid in 
    let price = prop |> Property.get_price |> string_of_int in
    if check_cpu h then last_cpu_auction name poss bid funds h prop cost st
    else last_player name poss funds prop cost price h st
  | h :: t -> let name = h |> Player.get_name in 
    let funds = h |> Player.get_money |> string_of_int in
    if get_money h <= bid 
    then let output_str = name ^ " does not have enough money to bid" in 
      if check_cpu h then buffer_cpu output_str else buffer output_str;
      auction st prop bid t
    else let poss = if name.[String.length name-1] = 's' then "'" else "'s" in
      let prop_nm = Property.property_name prop in
      let prce = prop |> Property.get_price |> string_of_int in
      if check_cpu h then cpu_auction name poss bid prce h prop bid t st
      else player_auction name prop poss players funds prop_nm prce bid h t st

and last_cpu_auction name poss bid funds h prop cost st = 
  let _ = buffer_cpu (name ^ poss ^ " " ^ "choice") in
  let cpu_buy_choice = bid <= int_of_string funds in 
  if cpu_buy_choice 
  then buy (get_player_key h st) prop cost st
  else st

and last_player name poss funds prop cost price h st = 
  let _ = buffer (name ^ poss ^ " " ^ "choice") in
  print_endline (game_status st);
  print_endline (player_status (get_player_key h st) st);
  print_endline (name ^ poss ^ " " ^ "choice");
  print_endline ("FUNDS: $" ^ funds);
  print_endline ("Would you like to buy " ^ Property.property_name prop 
                 ^ " for $" ^ string_of_int cost ^ "? (y/n)");
  print_endline ("Value: $" ^ price);
  let buy_choice = begin
    match prompt (fun x -> x="y" || x="n") "invalid\n" with 
    | "y" -> true
    | "n" -> false
    | _ -> failwith "auction buy_choice - never happens"
  end in 
  if buy_choice
  then buy (get_player_key h st) prop cost st
  else st

and cpu_auction name poss bid price h prop bid t st = begin 
  buffer_cpu (name ^ poss ^ " choice");
  let proposed_bid = bid + Random.int 50 in
  let cpu_bid = if Player.get_money h < proposed_bid then 0
    else proposed_bid in 
  if cpu_bid > int_of_string price || cpu_bid = 0
  then begin 
    buffer_cpu (Player.get_name h ^ " drops out");
    auction st prop bid t
  end
  else begin 
    buffer_cpu (Player.get_name h ^ " bids " ^ string_of_int cpu_bid); 
    auction st prop cpu_bid (t @ [h])
  end
end

and player_auction name prop poss players funds prop_name price bid h t st = 
  let _ = buffer (name ^ poss ^ " choice") in print_endline (game_status st);
  print_endline ("Current bidders: " ^ list_bidders players);
  print_endline (name ^ poss ^ " bid");
  print_endline ("FUNDS: $" ^ funds);
  print_endline ("How much for " ^ prop_name ^ "?");
  print_endline ("Value:\t\t$" ^ price);
  if bid=0 then () else print_endline ("Previous bid:\t$" ^ string_of_int bid);
  print_endline ("Enter your bid "^
                 "(enter 0 or q to be removed from the auction): "); 
  let new_bid = int_of_string_alph (prompt (fun x -> x="q" || x="e" || x="d" ||
                        (let x' = match int_of_string_opt x with
                              None -> -1 
                            | Some v ->
                              v in (x' > bid || x' = 0) && x' <= get_money h)) 
         "Bid must be greater than the previous bid and less than your \
          current funds! \nEnter 0 or q to be removed from the auction.") in
  match new_bid with 
  | 0 -> auction st prop bid t
  | x when x > bid -> auction st prop new_bid (t @ [h])
  | _ -> failwith "auction new_bid invalid"

(** [auction_order st acc pls] is the list of players [acc] from [pls] in [st] 
    in clockwise order starting 1 to the left of the current player in [st]. *)
let rec auction_order st acc = function 
  | [] -> acc
  | h :: t -> if get_player_key h st = current_player st 
    then t @ (acc @ [h])
    else auction_order st (acc @ [h]) t

(** [buy_or_auction state player property] is [st] after [player] chooses
    either to buy or auction unowned [property].  *)
let rec buy_or_auction state player property = 
  if check_cpu player
  then buy_or_auction_cpu state player property
  else begin
    let cost_string = property |> Property.get_price |> string_of_int in 
    print_buy_info property cost_string player;
    if (Player.get_money player) < Property.get_price property
    then begin 
      stall 2.; print_endline "you cannot afford this property"; stall 0.5;
      get_players state
      |> List.filter (fun x -> x |> Player.get_bankrupt |> not)
      |> auction_order state []
      |> auction state property 0
    end
    else begin
      let purchase_choice = acquire_purchase_choice 0 in 
      process_choice property player purchase_choice state
    end
  end

and buy_or_auction_cpu state player property = 
  let total_props = List.length (get_properties state) in
  let choice = 
    if property |> group |> get_gg_name |> String.lowercase_ascii = "utility" 
    || Property.get_price property + 100 >= Player.get_money player 
    then 2
    else if get_properties_bought state < total_props*4/5 
    then 1 else Random.int 4 in
  if choice = 1 then
    buy (current_player state) property (Property.get_price property) state
  else get_players state 
       |> List.filter (fun x -> x |> Player.get_bankrupt |> not)
       |> auction_order state [] 
       |> auction state property (2*Property.get_price property/7)

and print_buy_info property cost_string player = 
  print_endline (Property.property_name property ^ " is unowned");
  print_newline ();
  print_endline (deed property);
  print_endline ("COST:\t$" ^ cost_string);
  print_endline ("FUNDS:\t$" ^ 
                 (player |> Player.get_money |> string_of_int));

and acquire_purchase_choice _ = begin
  print_endline
    (String.concat "\n" 
       ["What would you like to do?"; 
        "1. Buy";"2. Auction";"3. Game Status";"4. Mortgage";"5. Sell"]);
  prompt (fun x -> check_prompt_range 1 5 x) "invalid\n" end

and process_choice property player purchase_choice state = 
  match purchase_choice with
  | "1" -> 
    buy (current_player state) property (Property.get_price property) state
  | "2" -> 
    get_players state
    |> List.filter (fun x -> x |> Player.get_bankrupt |> not)
    |> auction_order state []
    |> auction state property 0
  | "3" -> buy_or_auction state player property
  | "4" -> let state' = setup_mortgage state in 
    buy_or_auction state' player property
  | "5" -> let state' = prompt_build_sell_input false state in 
    buy_or_auction state' player property
  | _ -> failwith "purchase_choice out of range" 

let pay_tax player space st = 
  let current_player = get_player player st in 
  let payment = List.hd (get_rents space) in 
  let st' = 
    if payment > Player.get_money current_player
    then debted player payment ~-1 st 
    else
      let filtered_players = 
        List.filter (fun x -> player <> fst x) st.players in 
      let this_player = transact (~-1 * payment) player st in 
      { st with 
        players = List.sort_uniq cmp_fsts 
            ((player, this_player)::filtered_players) }
  in st'

(** [get_props_owned ord st] obtains properties owned by [ord] in [st]. *)
let get_props_owned ord st = 
  let props = st.properties in 
  let rec build_owned_string acc = function 
    | [] -> acc
    | h::t -> 
      if (Property.get_owner h = ord) then
        let new_str = acc ^ "\n" ^ Property.property_name h in 
        build_owned_string new_str t
      else build_owned_string acc t
  in build_owned_string "" props

(** [further_building state player] is CPU [player] making the 
    decision to build houses or mortgage certain properties it owns. *)
let rec further_building state = 
  let curr = current_player state in
  if Player.check_cpu (get_player curr state) 
  then let props_owned = get_owned curr state in
    let can_build_str = monopolies_owned state props_owned in 
    let gb = get_board state in
    let can_build' = List.map (fun x -> get_group_w_name x gb) can_build_str in 
    let can_build = List.filter (fun x -> get_gg_buildable x) can_build' in
    let build_sorted = List.sort_uniq (fun x y -> 
        Stdlib.compare (get_gg_build_cost x) (get_gg_build_cost y)) can_build 
    in if Random.int 5 = 0 && trading_enabled state
    then begin
      buffer_cpu (Player.get_name (get_player curr state)
                  ^" wants to trade!");
      setup_trade state
    end
    else build_on_this state props_owned build_sorted
  else state

and build_on_this state props_owned build_sorted = 
  match build_sorted with 
  | [] -> state
  | h :: t ->
    let props_to_build_on = List.filter (fun x -> 
        Property.group x = h) props_owned in 
    let props = List.rev (List.sort (fun x y -> 
        Stdlib.compare (get_price x) (get_price y)) 
        props_to_build_on) in
    build_on_check state 0 props

and build_on_check state cnt props = 
  let check_mortgaged = List.filter (fun x -> Property.mortgaged x) props in 
  match check_mortgaged with 
  | _::_ -> 
    print_endline "A property is mortgaged - cannot build"; 
    state 
  | [] -> build_on state cnt props 

and build_on state cnt = function
  | [] -> state
  | h::t -> 
    if cnt < (1 + List.length t) * 3
    then let player = get_player (current_player state) state in
      let threshold = 3 * (h |> group |> get_gg_build_cost) + 300 in
      if get_money player <= threshold
      then state
      else let curr = current_player state in 
        let st' = build_or_sell true curr h 1 state in 
        build_on st' (cnt+1) (List.append t [h])
    else state

(** [land_on st] is [st] after having handled landing on 
    the space that the current player is on *)
let rec land_on st = 
  let p = current_player st in
  let p_pos = get_player p st |> Player.get_position in
  if p_pos = (st |> get_board |> gtj_position) 
  then go_to_jail p st
  else begin
    let prop = get_prop_at p_pos st in 
    if Property.get_owner prop = ~-1 then subsequent_action st prop
    else if Property.get_owner prop = st.turn
    then begin 
      let prop_name = Property.property_name prop in
      print_endline ("You own " ^ prop_name); st 
    end
    else if Property.mortgaged prop
    then begin print_endline "This property is mortgaged"; st end
    else begin
      print_endline "You must pay rent!\n"; 
      pay_rent (current_player st) prop st 
    end
  end

and go_to_jail p st = begin
  print_endline ("landed on go to jail\nstraight to jail for you!");
  let player = get_player p st in
  let player' = 
    player |> Player.set_position (jail_pos st.board) |> toggle_jail in
  let f_players = filter_players st p in
  {st with players = (p,player')::f_players}
end

(** [subsequent_action state property] is the change in game state
    where actions are taken based on the group of the user's space. *)
and subsequent_action state property = 
  let current_pl = current_player state in 
  let player = get_player current_pl state in 
  let current_pos = Player.get_position player in 
  let current_space = get_space current_pos (get_board state) in 
  let group_name = space_group_name current_space in 
  let st' = match group_name with 
    | "tax" -> 
      print_endline "You must pay tax!";
      pay_tax current_pl current_space state
    | "chance" -> 
      print_endline "This is your Chance card:\n";
      draw_card "chance" state
    | "chest" -> 
      print_endline "This is your Community Chest card:\n";
      draw_card "community chest" state
    | "go" | "free" -> state 
    | _ -> buy_or_auction state player property
  in further_building st'

and draw_card deck st = 
  let card = get_card_from_deck deck st in 
  let st' = action_on_card card st in 
  let type_card = Card.get_card_type card in 
  let new_st = if deck = "chance" && type_card <> "get_out_jail"
    then {st' with chance_cards = match st'.chance_cards with 
        | h::t -> t @ [h]
        | [] -> failwith "always exists Chance cards - no goj"} 
    else if deck = "chance" && type_card = "get_out_jail"
    then {st' with chance_cards = match st'.chance_cards with 
        | h::t -> t 
        | [] -> failwith "always exists Chance cards - goj"} 
    else if deck = "community chest" && type_card <> "get_out_jail"
    then {st' with chest_cards = match st'.chest_cards with 
        | h::t -> t @ [h]
        | [] -> failwith "always exists Community Chest cards - no goj"}
    else if deck = "community chest" && type_card = "get_out_jail" 
    then {st' with chest_cards = match st'.chest_cards with 
        | h::t -> t
        | [] -> failwith "always exists Community Chest cards - goj"} 
    else st' in inc_chance_rotation new_st

and get_card_from_deck deck st =
  match deck with 
  | "chance" -> 
    print_endline (st |> get_chance_cards |> List.hd 
                   |> Card.get_card_description); 
    List.hd st.chance_cards
  | "community chest" -> 
    print_endline (Card.get_card_description (List.hd st.chest_cards)); 
    List.hd st.chest_cards
  | _ -> failwith "card is not in either deck"

(** [action_on_card card st] is [st] after actions from [card] are taken. *)
and action_on_card card st = 
  stall 0.5;
  let todo = Card.get_card_type card in 
  let a = Card.get_card_amount card in
  let cp = current_player st in
  match todo with
  | "raise" -> increase_money card a cp st 
  | "lose" -> decrease_money card a cp st
  | "lose_player" -> lose_money_each_player card a cp st
  | "earn_player" -> gain_money_each_player card a cp st
  | "repair" -> change_money_repair card a cp st  
  | "movetojail" -> move_to_jail_helper st
  | "moveto" -> let gb_size = Gameboard.size st.board in
    let player_pos = (Player.get_position (get_player st.turn st)) in
    move_to_helper a st gb_size player_pos
  | "move_dist" -> let gb_size = Gameboard.size st.board in
    let player_pos = (Player.get_position (get_player st.turn st)) in
    move_dist_back_helper a st gb_size player_pos
  | "get_out_jail" -> get_out_jail_helper card st 
  | _ -> failwith "card has no corresponding action"

(** [increase_money card a cp st] increases the current player's money 
    by the amount specified on the card *)
and increase_money card a cp st = 
  match a with 
  | Integer i -> 
    {st with 
     players = 
       let curr_player = current_player st in 
       let players_list = filter_players st curr_player in 
       let new_player = transact i cp st in 
       List.sort_uniq cmp_fsts 
         ((curr_player, new_player)::players_list)
    }
  | _ -> failwith "amount to change not of integer type"

(** [decrease_money card a cp st] decreases the current player's money 
    by the amount specified on the card *)
and decrease_money card a cp st = 
  let amt = match a with
    | Integer i -> i
    | _ -> failwith "amount to change not of integer type" in
  let curr_player = current_player st in 
  let players_list = filter_players st curr_player in 
  if (st |> get_player curr_player |> get_money) >= amt 
  then
    let new_player = transact (-1 * amt) cp st in 
    let players' = List.sort_uniq cmp_fsts 
        ((curr_player, new_player)::players_list) in
    { st with 
      players = players' }
  else 
    st |> debted curr_player amt (-1)

(** [lose_money_each_player card a cp st] has the current player 
    give a certain amount of money to each other player in play 
    the amount is specified by [card] *)
and lose_money_each_player card a cp st = 
  let amt = match a with
    | Integer i -> i
    | _ -> failwith "amount to change not of integer type" in 
  let curr = st.turn in 
  let player = get_player curr st in 

  (* [process_players idx st'] is [st] with [idx] money added or removed from
     one player and the reverse to the others. *)
  let rec process_players idx st' =  
    if idx >= List.length st'.players 
    then st' 
    else begin 
      if idx = curr || (st' |> get_player idx |> get_bankrupt)
      then process_players (idx+1) st' 
      else if get_money player >= amt 
      then 
        let filt = List.filter (fun x -> 
            fst x<>curr && fst x<>idx) st'.players in
        let new_player = transact (-1 * amt) curr st' in
        let new_opponent = transact amt idx st' in
        let _ = print_newline () in
        process_players (idx+1)
          { st' with players = (curr,new_player) :: (idx,new_opponent) :: filt}
      else process_players (idx+1) (debted curr amt idx st') 
    end
  in process_players 0 st

(** [gain_money_each_player card a cp st] has the current player receive 
    money from each player in play the amount is specified by [card] *)
and gain_money_each_player card a cp st = 
  let amt = match a with
    | Integer i -> i
    | _ -> failwith "amount to change not of integer type" in 
  let curr = st.turn in 
  let rec process_players idx st' =  
    if idx >= List.length st'.players then st' 
    else let opponent = List.assoc idx st'.players in 
      if idx = curr || (st' |> get_player idx |> get_bankrupt)
      then process_players (idx+1) st' 
      else if get_money opponent >= amt 
      then let filt = List.filter (fun x -> 
          fst x<>curr && fst x<>idx) st'.players in
        let new_opponent = transact (-1 * amt) idx st' in
        let new_player = transact amt curr st' in
        let _ = print_newline () in
        process_players (idx+1)
          { st' with players = (curr,new_player) :: (idx,new_opponent) :: filt}
      else process_players (idx+1) (debted idx amt curr st') in 
  process_players 0 st

(** [change_money_repair card a cp st] is [st] after the current player has 
    paid an amount depending on the amount of buildings in play 
    chance: every building owned by the current player
    chest: every building on the board *)
and change_money_repair card a cp st = 
  match a with 
  | Tuple (house, hotel) -> 
    let (pents, temps) = count_properties card st (get_properties st) in
    let curr_player = current_player st in 
    let player = get_player curr_player st in 
    let filt = filter_players st curr_player in
    let amt = pents * house + temps * hotel in
    if get_money player >= amt
    then let new_player = 
           if pents = 0 && temps = 0 then player
           else transact (-1 * amt) cp st in 
      let players' = List.sort_uniq cmp_fsts 
          ((curr_player, new_player) :: filt) in
      { st with
        players = players' }
    else 
      debted curr_player amt (-1) st
  | _ -> failwith "card amount type not valid for repairs"

(** [count_properties card st props] returns a pair where the first 
    is the amount of houses that the player has to pay for and the second 
    is the amount of hotels that they have to pay for *)
and count_properties card st props =
  match Card.get_card_deck card with
  | "chest" -> (get_house_count st, get_hotel_count st)
  | "chance" -> let buildings_owned =
                  props |> List.filter (fun x -> get_owner x = st.turn)
                  |> List.map buildings |> List.fold_left (@) [] in 
    let rec get_house_count = function 
      | [] -> 0 
      | h :: t -> if h=House 
        then 1 + get_house_count t 
        else get_house_count t in 
    let rec get_hotel_count = function 
      | [] -> 0 
      | h :: t -> if h=Hotel 
        then 1 + get_hotel_count t 
        else get_hotel_count t in 
    (get_house_count buildings_owned, get_hotel_count buildings_owned)
  | _ -> failwith "well, no"

(** [move_to_jail_helper st] is [st] with the current player 
    moved to jail *)
and move_to_jail_helper st = 
  let player = List.assoc st.turn st.players 
               |> set_position (jail_pos st.board) 
               |> toggle_jail in 
  let filt = filter_players st st.turn in 
  let name = get_name player in 
  print_endline (name ^ " is now in jail");
  {st with players = (st.turn,player) :: filt;
           can_end = true}

(** [move_dist_back_helper a st gb_size init_pos] is [st] with the player 
    moved backwards by the amount specified in [a] *)
and move_dist_back_helper a st gb_size init_pos = 
  let amt = match a with 
      Integer i -> i | _ -> failwith "not correct amount type" in
  let size = Gameboard.size st.board in
  let next_pos =
    init_pos - amt 
    |> (fun x -> if x<0 then x+size else x) in
  let player =
    List.assoc st.turn st.players
    |> set_position next_pos in 
  let filt = filter_players st st.turn in 
  let new_st = { st with players = (st.turn,player) :: filt }
               |> move st.turn 0 in 
  new_st |> land_on

(** [move_to_helper a st gb_size player_pos] is [st] with the current player
    moved to the position specified in [a] *)
and move_to_helper a st gb_size init_pos = 
  match a with 
  | Integer next_pos ->
    let player = List.assoc st.turn st.players
      |> set_position next_pos 
      |> (fun x -> if next_pos < init_pos then change_money 200 x else x) in 
    let filt = filter_players st st.turn in 
    { st with players = (st.turn,player) :: filt } 
    |> move st.turn 0 |> land_on
  | Group s -> let props = get_spaces st.board in 
    let list_of_s = List.filter (fun x -> (space_group_name x) = s) props in 
    let num_move = 
      find_closest_s st init_pos (Gameboard.size st.board) list_of_s in
    let next_pos = init_pos + num_move in
    let player =
      List.assoc st.turn st.players |> set_position next_pos 
      |> (fun x -> if next_pos < init_pos then change_money 200 x else x) in 
    let filt = filter_players st st.turn in 
    { st with players = (st.turn,player) :: filt } 
    |> move st.turn 0 |> land_on
  | _ -> failwith "not correct amount type"

(** [find_closest_s st player_pos acc lst] is the minimum number of spaces to 
    move in order to move to the closest space in [lst]. *)
and find_closest_s st player_pos acc = function
  | [] -> acc
  | h::t -> 
    let spc_ordinal = space_pos h in 
    let spaces_off = spc_ordinal - player_pos in
    if (abs spaces_off) < (abs acc) 
    then find_closest_s st player_pos spaces_off t
    else find_closest_s st player_pos acc t

(** [next_move_s st' player_pos list_of_s] is [st'] with player corresponding
    to [player_pos] having bought, auctioned, or paid rent for landing on
    a new unbuildable property. *)
and next_move_s st' player_pos list_of_s = function 
  | "utility" -> next_move_utility player_pos st'
  | "rail" -> next_move_rail player_pos st' 
  | _ -> failwith "next_move_s shouldn't reach this case for the real board"

and next_move_utility player_pos st'= 
  let prop = get_prop_at player_pos st' in 
  let owner = Property.get_owner prop in 
  if owner = ~-1
  then ask_about_buy st' prop
  else if owner = current_player st' then st'
  else
    let s_die = st' |> get_dice |> fst |> List.hd in
    let s_roll_val = roll_speed_die s_die in
    let roll_values = roll_helper (st' |> get_dice |> snd) [] in 
    print_rolls roll_values;
    print_endline ("Speed Die: " ^ str_s_roll s_roll_val);
    let s_int = 
      match s_roll_val with 
      | SInt i -> i 
      | _ -> 0 in
    let sum = List.fold_left (+) 0 roll_values + s_int in
    let payment = 10 * sum in
    print_endline ("You rolled " ^ string_of_int sum ^
                   ". You will pay $" ^ string_of_int payment ^ "."); 
    pay_rent_special payment owner st';

and next_move_rail player_pos st' = 
  let prop = get_prop_at player_pos st' in 
  let owner = Property.get_owner prop in 
  if owner = ~-1 then ask_about_buy st' prop
  else let rails =
         List.filter 
           (fun x -> get_gg_name (Property.group x) = "rail") st'.properties in
    let owner_owned_rails = List.filter 
        (fun x -> Property.get_owner x = owner) rails in
    let num_rails = List.length owner_owned_rails in 
    let payment = 2 * 25 * (pow 2 (num_rails-1)) in
    let infl = if num_rails = 1 then "" else "s" in  
    print_endline ("The owner of this property owns " ^
                   string_of_int (List.length owner_owned_rails)
                   ^ " railroad" ^ infl
                   ^ ". You will pay $" ^ string_of_int payment ^ "."); 
    pay_rent_special payment owner st';

    (** [pay_rent_special payment owner st'] pays different rent given
        whether it has landed on a railroad or a utility. *)
and pay_rent_special payment owner st' = 
  {st' with
   players = 
     let holder' = get_player owner st' in 
     let player_ord = current_player st' in
     let filt_players = List.filter 
         (fun x -> (fst x) <> owner || (fst x) <> player_ord) st'.players in
     let player' = get_player player_ord st' in 
     let holder = change_money payment holder' in 
     let player = change_money (-1*payment) player' in 
     List.sort_uniq cmp_fsts 
       ((owner, holder)::(player_ord,player)::filt_players)
  }

(** [ask_about_buy st' prop] prompts current player in [st] 
    to buy or auction property it has moved to from card. *)
and ask_about_buy st' prop =
  let p_ord = current_player st' in 
  let p = get_player p_ord st' in 
  buy_or_auction st' p prop

(** [get_out_jail_helper card st] adds the "Get Out of Jail Free" card
    to current player in [st]. *)
and get_out_jail_helper card st =
  {st with 
   players =
     let curr_player = current_player st in 
     let player = get_player curr_player st in 
     let players = filter_players st curr_player in
     let new_player = add_to_inventory card player in  
     List.sort_uniq cmp_fsts ((curr_player, new_player)::players)
  }

(** [dest_str idx roll st] is the string detailing where the current player in 
    [st] will be after moving [roll] spaces, as an option corresponding 
    to [idx]. *)
let dest_str idx roll st = 
  let pos = get_player (current_player st) st |> Player.get_position in
  let dest_name = st |> get_board |> get_space (pos + roll) |> space_name in 
  string_of_int idx^". Move "^string_of_int roll^" spaces to "^dest_name

(** [print_bus_opts index rolls sum st] prints the options for a player in 
    [st] who rolls a bus on the speed die along with [rolls] on the normal 
    dice, with [sum] being the total rolled. [index] is the starting index 
    for the options. *)
let rec print_bus_opts index rolls sum st = 
  match rolls with 
  | [] -> print_endline (dest_str index sum st)
  | h :: t -> begin 
      print_endline (dest_str index h st);
      print_bus_opts (index + 1) t sum st 
    end

let rec bus_choice rolls st = 
  print_endline "You rolled a bus! Choose how far you want to move: ";
  let r1 = List.hd rolls in 
  let r2 = List.nth rolls 1 in
  let rsum = r1 + r2 in
  print_bus_opts 1 rolls rsum st;
  print_endline "4. Game Status";
  let choice = prompt (check_prompt_range 1 4) "invalid selection\n" in
  match choice with
  | "1" -> string_of_int r1
  | "2" -> string_of_int r2
  | "3" -> string_of_int (r1 + r2)
  | "4" -> begin print_endline (game_status st); 
      bus_choice rolls st end
  | _ -> failwith "prompt_bus_choice invalid choice" 

(** [prompt_bus_choice rolls st] prompts the current player in [st] for a 
    choice of how far to move when they have rolled a [Bus] on the speed die. 
    The player can choose to move equal to either or both of the [rolls]. *)
let rec prompt_bus_choice rolls st = 
  let curr = current_player st in
  let r1 = List.hd rolls in 
  let r2 = List.nth rolls 1 in
  let rsum = r1 + r2 in
  let doubles = check_doubles rolls in
  let st' = if not doubles 
    then {st with can_end = true} 
    else {st with dcount = get_dcount st + 1} in
  if get_dcount st' >= 3 
  then begin print_endline "3 doubles, go to jail!"; 
    move_to_jail_helper st' end
  else let choice = if check_cpu (get_player curr st)
         then cpu_bus_choice r1 r2 rsum curr st
         else bus_choice rolls st in
    move (current_player st) (int_of_string choice) st'

and cpu_bus_choice v1 v2 vs curr st = 
  let player = get_player curr st in 
  let curr_pos = Player.get_position player in
  let int1 = curr_pos + v1 in 
  let int2 = curr_pos + v2 in 
  let int_sum = curr_pos + vs in 
  let tot_props = get_properties st in
  let corr_prop = List.filter (fun x -> 
      let board_num = Property.board_ordinal x in 
      board_num = int1 || board_num = int2 || board_num = int_sum) tot_props in 
  let should_go = List.filter (fun x -> 
      Property.get_owner x = curr || Property.get_owner x = -1) corr_prop in 
  if should_go = [] then "3"
  else if List.length should_go = 2 
  then if Property.board_ordinal (List.nth should_go 1) - curr_pos = int2
    then "2" 
    else if Property.board_ordinal (List.hd should_go) - curr_pos = int1  
    then "1" 
    else "3"
  else "3"

(** [str_of_spaces idx acc spaces] is the indexed string representation of 
    [spaces] with each space name given an incrementing index [idx]. *)
let rec str_of_spaces idx acc = function
  | [] -> acc
  | h :: t -> 
    let acc' = acc ^ string_of_int idx ^ ". " ^ property_name h ^ "\n" in
    str_of_spaces (idx+1) acc' t

(** [move_direct dest st] moves the current player in [st] to space number 
    [dest] and allows them to interact with the space after checking if 
    they have passed GO. *)
let move_direct dest st = 
  let pl = current_player st in
  let player = get_player pl st in
  let moved = set_position dest player in
  let passed_go =  
    if pl > Player.get_position moved 
    then begin
      print_endline "Collect $200 for passing GO";
      change_money 200 moved
    end
    else moved in
  let f_players = filter_players st pl in
  let new_state = {st with players = (pl, passed_go) :: f_players} in
  new_state

(** [sort_props st] is the sorted list of spaces in [st] *)
let sort_props st = 
  List.sort (fun x y ->  compare (board_ordinal x) (board_ordinal y)) 
    (get_properties st)

(** [prompt_triples_choice st] prompts the player for their choice of which 
    space on the board in [st] to move to and moves them there. *)
let rec prompt_triples_choice st = 
  let props = st |> sort_props in
  print_endline (game_status st);
  print_endline "You rolled triples! Where would you like to go?";
  print_endline (str_of_spaces 1 "" props);
  let max_choice = List.length props in
  let choice = 
    let curr = current_player st in
    let player = get_player curr st in 
    if check_cpu player 
    then cpu_choice st
    else prompt (check_prompt_range 1 max_choice)
        "invalid space selection\n" in 
  let num_choice = int_of_string choice - 1 in 
  let new_st = st |> move_direct (num_choice) in 
  {new_st with can_end = true}

and cpu_choice st = 
  let props = st |> get_properties in 
  let unowned = List.filter (fun x -> Property.get_owner x = -1) props in 
  if unowned = [] then "1" 
  else let priorities = priority_organizing st in 
    let rec create_priority_buy priorities acc props = 
      match priorities with 
      | [] -> acc
      | h::t ->
        let p = List.filter (fun x -> Property.group x = h) props in 
        create_priority_buy t (acc@p) unowned in 
    let spc = create_priority_buy priorities [] props 
              |> List.hd |> board_ordinal in 
    string_of_int (spc-1)

let man_move_check prop = 
  let g = prop |> group |> get_gg_name in
  not (g="chest" || g="free" || g="go" || g="chance" || g="tax")

let man_move_unmortgaged_check st prop = 
  get_owner prop <> current_player st
  && man_move_check prop
  && not (mortgaged prop)

let man_move_unmortgaged props st = 
  match List.find_opt (man_move_unmortgaged_check st) props with 
  | None -> begin 
      print_endline "All opponents' properties are mortgaged \nDid not move";
      st 
    end
  | Some prop -> move_direct (board_ordinal prop) st

(** [man_move st] is [st] with the current player moving according to the speed 
    die rules for the Monopoly Man roll and interacting with whatever space 
    they land on.
    The player must move to the next unowned property, which they may buy or 
    auction. If there are no unowned properties, the player moves to the 
    nearest unmortgaged property owned by an opponent. If all opponents' 
    properties are mortgaged, do nothing. *)
let man_move st doubles =
  print_endline "\nYou rolled Monopoly Man!\n\
                 Moving to nearest unowned property...";
  let pos = get_player (st |> current_player) st |> Player.get_position in
  let props = st |> sort_props |> rotate_left (pos+1) in
  match List.find_opt (fun x -> 
      get_owner x = -1 && man_move_check x) props with
  | Some prop -> 
    move_direct (board_ordinal prop) st
  | None -> begin 
      print_endline "All properties owned.\n\
                     Moving to opponents' nearest unmortgaged property..."; 
      man_move_unmortgaged props st
    end

(** [speed_turn rolls p s_int state] is [roll] with the result of player number 
    [p] rolling [rolls] on the normal dice and [s_int] on the speed die in 
    [state]. *)
let speed_turn rolls p s_int state =
  let sum = List.fold_left (+) 0 rolls + s_int in
  let doubles = check_doubles rolls in
  let new_state = 
    if doubles then {state with dcount = state.dcount + 1} else state in
  let moved_state = if new_state.dcount = 3 
    then begin print_endline ("3 doubles - go to jail!");
      move_to_jail_helper new_state end
    else if get_can_end new_state then new_state 
    else new_state |> move p sum in
  let new_player = get_player p moved_state in 
  if doubles && not (check_jail new_player) 
  then {moved_state with can_end = false} 
  else {moved_state with can_end = true}

(** [speed_roll state] is [roll state] with additional speed die rules *)
let speed_roll state = 
  let s_die = state |> get_dice |> fst |> List.hd in
  let s_roll_val = roll_speed_die s_die in
  let roll_values = roll_helper (state |> get_dice |> snd) [] in 
  print_rolls roll_values;
  print_endline ("Speed Die: " ^ str_s_roll s_roll_val);
  let s_int = match s_roll_val with | SInt i -> i | _ -> 0 in
  let sum = List.fold_left (+) 0 roll_values + s_int in
  let state' = {state with roll_value = sum} in
  let p = current_player state' in 
  let doubles = check_doubles roll_values in
  let triples = doubles && check_triples roll_values s_int in
  if triples then prompt_triples_choice state'
  else match s_roll_val with 
    | Man -> 
      let new_state = state' |> speed_turn roll_values p s_int |> land_on in
      if not (get_player p new_state |> check_jail) 
      then man_move new_state doubles
      else begin print_endline "In Jail - cannot move."; new_state end
    | Bus -> prompt_bus_choice roll_values state' 
    | SInt _ -> speed_turn roll_values p s_int state'

let rec roll state = 
  let p = current_player state in
  let player' = get_player p state in
  if speed_die_enabled state && not (check_jail player')
  then speed_roll state
  else roll_normal p player' state

and roll_normal p player' state = 
  let values = roll_helper (state |> get_dice |> snd) [] in print_rolls values;
  let sum = List.fold_left (+) 0 values in
  let doubles = check_doubles values in 
  let released = doubles && check_jail player' in 
  let player = make_player released player' in
  let new_state' = make_state p player sum state in
  let new_state = if doubles && not (check_jail player) && not released
    then {new_state' with dcount = state.dcount + 1}
    else {new_state' with dcount = 0} in
  let moved_state = make_moved_state p player released sum state new_state in 
  let new_player = get_player p moved_state in 
  let final_state = if released then {moved_state with can_end = true}
    else if doubles && not (check_jail new_player) 
    then {moved_state with can_end = false}
    else {moved_state with can_end = true} in 
  if (get_jail_turn_count new_player) >= 3 && check_jail new_player
  then pay_out_jail (current_player final_state) final_state else final_state

and make_moved_state p player released sum state new_state = 
  if new_state.dcount = 3 
  then begin print_endline ("3 doubles - go to jail!");
    let jailed_state = {new_state with players = begin
        let f_players = filter_players state p in 
        let jailed_player = 
          player |> toggle_jail |> set_position (jail_pos state.board) in 
        let mod_players = (p,jailed_player) :: f_players in
        List.sort_uniq cmp_fsts mod_players end;
           can_end = true;} in jailed_state end
  else if check_jail player 
  then begin print_endline "can't move\n"; new_state end
  else if get_can_end new_state then new_state
  else if released then new_state else move p sum new_state

and make_player released player' = 
  if released then begin
    print_string "got out of jail!\n";
    player' |> toggle_jail |> reset_jail_turn_count 
  end
  else player' 

and make_state p player sum state = 
  {state with players = begin
       let f_players = filter_players state p in 
       let mod_players = (p,player) :: f_players in
       List.sort_uniq cmp_fsts mod_players
     end;
          dcount = 0;
          roll_value = sum;
  }

let roll_dice current_name state = 
  print_string current_name; print_endline " rolls";
  let pos = 
    state |> get_player (current_player state) |> Player.get_position in
  let state' = state |> roll in 
  let new_pos = 
    state' |> get_player (current_player state) |> Player.get_position in
  if pos = new_pos
  then state'
  else land_on state'

let inventory player st = 
  let p = get_player player st in 
  get_inventory p

let use player st = 
  let p = get_player player st in 
  let cards = get_inventory p in 
  if cards = [] 
  then begin
    print_endline ("You do not have a Get Out of Jail Card.\n"); st
  end
  else let card = List.hd cards in
    let f_players = filter_players st player  in 
    let mod_player' = (remove_from_inventory card p) in 
    let mod_player = toggle_jail mod_player' in
    let players' = List.sort_uniq cmp_fsts 
        ((player, mod_player) :: f_players) in
    let st' = if Card.get_card_deck card = "chance" then
        {st with players = players';
                 chance_cards = List.append st.chance_cards [card];}
      else 
        {st with players = players';
                 chest_cards = List.append st.chest_cards [card];} in 
    buffer_cpu "You have used your Get Out of Jail Card.";
    reset_jtc player st'

(** [properties_from_channel chnl] is the list of properties generated
    from input channel [chnl]. Finishes creating properties when it encounters
    the [END] keyword *)
let rec properties_from_channel chnl = 
  match input_line chnl with 
  | "END" -> [] 
  | _ as s -> (Property.property_from_string s) :: properties_from_channel chnl

(** [players_from_channel chnl] is the list of players generated
    from input channel [chnl]. Finishes creating players when it encounters
    the [END] keyword *)
let rec players_from_channel chnl =  
  match input_line chnl with 
  | "END" -> [] 
  | _ as s -> (player_from_string s) :: players_from_channel chnl

(** [dice_from_channel chnl] is the list of dice generated
    from input channel [chnl]. Finishes creating dice when it encounters
    the [END] keyword *)
let rec dice_from_channel chnl =  
  match input_line chnl with 
  | "END" -> [] 
  | _ as s -> (Die.die_from_string s) :: dice_from_channel chnl

let e_winner st = 
  let rec check_win pl = function 
    | [] -> true 
    | (k,v) :: t -> 
      ((k=pl) && check_win pl t)
      || ((get_bankrupt v) && check_win pl t)
  in 
  check_win st.turn (st.players)

(** [state_from_file' fn] is the implemetation state_from_file *)
let rec state_from_file' (fn:string) = 
  let chnl = open_in fn in 
  let board_name = input_line chnl in 
  assert ((input_line chnl) = "END");
  let dice = dice_from_channel chnl in 
  let players = players_from_channel chnl in 
  let properties = properties_from_channel chnl in 
  let dcount = (input_line chnl) |> int_of_string in
  let speed_die = (input_line chnl) |> bool_of_string in 
  let trade = (input_line chnl) |> bool_of_string in 
  let can_end = (input_line chnl) |> bool_of_string in 
  let turn = (input_line chnl)  |> int_of_string in 
  let roll_value = (input_line chnl) |> int_of_string in 
  let game_over = (input_line chnl) |> bool_of_string in 
  let props_bought = (input_line chnl) |> int_of_string in 
  let tc = (input_line chnl) |> int_of_string in
  let seed = (input_line chnl) |> int_of_string in 
  let chr = (input_line chnl) |> int_of_string in 
  let ccr = (input_line chnl) |> int_of_string in
  let gb = from_json board_name (Yojson.Basic.from_file (board_name)) in 
  create_state players dice gb dcount (speed_die,trade) fn seed properties chr 
    ccr can_end turn roll_value game_over props_bought tc

and create_state players dice gb dcount (speed_die,trade) fn
    seed properties chr ccr can_end turn roll_value game_over props_bought tc = 
  let init = init_state players dice gb (speed_die,trade) fn seed in
  { init with properties = properties;
              chance_cards = rotate_left chr init.chance_cards;
              chest_cards = rotate_left ccr init.chest_cards;
              dcount = dcount;
              can_end = can_end;
              turn = turn;
              roll_value = roll_value;
              game_over = game_over;
              filename = begin 
                try let idx = String.index init.filename '-' in 
                  String.sub init.filename 0 idx with
                  Not_found -> String.sub init.filename 0 
                                 (~-4 + String.length init.filename) end;
              properties_bought = props_bought;
              turn_count = tc;
              chance_rot = chr;
              chest_rot = ccr;
  }

let state_from_file fn = 
  try state_from_file' fn with 
    Failure _ -> raise (Invalid_save (fn ^ " is not a valid .sav file"))

(** [get_properties_string properties] is the multi-line string representing
    all of the properties in accordance with property.property_to_string *)
let rec get_properties_string = function
  | [] -> "" 
  | h :: [] ->
    (Property.property_to_string h)
  | h1 :: (h2 :: t2 as t) ->
    (Property.property_to_string h1) ^ "\n" ^ (get_properties_string t)

(** [get_players_string players] is the multi-line string representing
    all of the players in accordance with Player.player_to_string 
    these individual player strings are also preceeded by the 
    players respective ordinal int*)
let rec get_players_string = function
  | [] -> "" 
  | (k,v) :: [] ->
    (string_of_int k) ^ ";"
    ^ (player_to_string v)
  | (k1,v1) :: (h2 :: t2 as t) ->
    (string_of_int k1) ^ ";"
    ^ (player_to_string v1) ^ "\n" ^ (get_players_string t)

(** [get_dice_string dice] is the multi-line string representing
    all of the dice in accordance with Dice.die_to_string*)
let rec get_dice_string = function
  | [] -> "" 
  | h :: [] ->
    (Die.die_to_string h)
  | h1 :: (h2 :: t2 as t) ->
    (Die.die_to_string h1) ^ "\n" ^ (get_dice_string t)

let rec state_to_file st = begin
  let fn = st.filename ^ ".sav" in 
  ({|echo "" > |} ^ fn) |> Sys.command |> ignore;
  let chnl = Stdlib.open_out fn in 
  let gb_string = get_gb_name st.board in
  write_string chnl (gb_string ^ "\n") "END\n";
  let dice_string = get_dice_string st.dice in 
  write_string chnl (dice_string ^ "\n") "END\n";
  let players_string = get_players_string st.players in
  write_string chnl (players_string ^ "\n") "END\n"; 
  let properties_string = get_properties_string (sort_props st) in 
  write_string chnl (properties_string ^ "\n") "END\n";
  string_output chnl st
end

and string_output chnl st = begin
  write_string chnl (st.dcount |> string_of_int) "\n";
  write_string chnl (st.flags |> fst |> string_of_bool) "\n";
  write_string chnl (st.flags |> snd |> string_of_bool) "\n";
  write_string chnl (st.can_end |> string_of_bool) "\n";
  write_string chnl (st.turn |> string_of_int) "\n";
  write_string chnl (st.roll_value |> string_of_int) "\n";
  write_string chnl (st.game_over |> string_of_bool) "\n";
  write_string chnl (st.properties_bought |> string_of_int) "\n";
  write_string chnl (st.turn_count |> string_of_int) "\n";
  write_string chnl (st.seed |> string_of_int) "\n";
  write_string chnl (st.chance_rot |> string_of_int) "\n";
  write_string chnl (st.chest_rot |> string_of_int) "\n"; 
  flush chnl
end

and write_string chnl f txt = begin
  output_string chnl f;
  output_string chnl txt;
end

(** [four_dig n] is a string representing the integer [n] using 
    four decimal digits.
    FAILS when given a number with more than four digits*)
let four_dig n = 
  let s = string_of_int n in 
  match String.length s with
  | 1 -> "000" ^ s
  | 2 -> "00" ^ s
  | 3 -> "0" ^ s
  | 4 -> s
  | _ -> failwith "over 9999 turns have passed"

(** [get_filenames ()] is the string list of all availible
    files and directories *)
let get_filenames _ = 
  let handle = opendir "." in 
  let rec get_names acc = 
    try
      let filename = readdir handle in 
      get_names (filename::acc)
    with End_of_file -> acc in 
  get_names []

let save_turn st = 
  if List.mem st.filename (get_filenames ())
  then () 
  else ("mkdir " ^ st.filename) |> Sys.command |> ignore;
  let st' = {
    st with
    filename = st.filename
               ^ Filename.dir_sep
               ^ st.filename
               ^ "-"
               ^ (four_dig st.turn_count)
  } in 
  state_to_file st' 

let load_turn turn fn = 
  let path =
    fn
    ^ Filename.dir_sep
    ^ fn
    ^ "-"
    ^ (four_dig turn)
    ^ ".sav" in 
  let st' = state_from_file path in 
  { st' with filename = fn }
