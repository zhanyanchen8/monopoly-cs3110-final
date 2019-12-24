open Property
open Card

type t = {
  name : string;
  is_cpu : bool;
  money : int;
  position : int;
  jail : bool;
  inventory : Card.t list;
  bankrupt : bool;
  jail_turn_count : int;
}

let create_player name cpu = {
  name = name;
  is_cpu = cpu;
  money = 1500;
  position = 0;
  jail = false;
  inventory = [];
  bankrupt = false;
  jail_turn_count = 0;
}

let get_name player =
  player.name

let check_cpu player =
  player.is_cpu

let get_money player =
  player.money

let change_money amt player = 
  if player.money + amt < 0
  then failwith "projected negative money"
  else if player.money >= (-1 * amt) then {
    player with money = player.money + amt
  }
  else player

let reset_money player = {
  player with money = 0
}

let get_position player =
  player.position

let set_position pos player = 
  if pos < 0 
  then failwith "Illegal move to a negative position"
  else {player with position = pos}

let check_jail player =
  player.jail

let toggle_jail player = {
  player with jail = not player.jail;
} 

let get_bankrupt player = 
  player.bankrupt

let toggle_bankrupt player = {
  player with bankrupt = not player.bankrupt;
} 

let inc_jail_turn_count player = {
  player with jail_turn_count = player.jail_turn_count + 1;
}


let reset_jail_turn_count player = {
  player with jail_turn_count = 0;
}

let get_jail_turn_count player = 
  player.jail_turn_count


let add_to_inventory card player = {
  player with inventory = card :: player.inventory;
} 

let remove_from_inventory card player = 
  match player.inventory with
  | [] -> player
  | h :: t -> {
      player with inventory = 
        if Card.get_card_name h = Card.get_card_name card 
        then t else h::t
    }

let get_inventory player = 
  player.inventory

let cards_from_string = function 
  | "" -> [] 
  | _ as s -> begin
      let seg = String.split_on_char '|' s in 
      let rec process_segs = function 
        | [] -> [] 
        | h :: t -> (Card.card_from_string h) :: process_segs t in 
      process_segs seg
    end

let player_from_string s = 
  let ord_head = String.split_on_char ';' s in 
  let ord = (List.hd ord_head) |> int_of_string in 
  let name_head = List.tl ord_head in 
  let name = List.hd name_head in 
  let is_cpu_head = List.tl name_head in 
  let is_cpu = (List.hd is_cpu_head) |> bool_of_string in 
  let money_head = List.tl is_cpu_head in 
  let money = (List.hd money_head) |> int_of_string in
  let position_head = List.tl money_head in 
  let position = (List.hd position_head) |> int_of_string in 
  let jail_head = List.tl position_head in 
  let jail = (List.hd jail_head) |> bool_of_string in 
  let inventory_head = List.tl jail_head in 
  let inventory = (List.hd inventory_head) |> cards_from_string in 
  let bankrupt_head = List.tl inventory_head in 
  let bankrupt = (List.hd bankrupt_head) |> bool_of_string in 
  let jtc_head = List.tl bankrupt_head in 
  let jtc = (List.hd jtc_head) |> int_of_string in 
  ord, {
    name = name; is_cpu = is_cpu; money = money;
    position = position; jail = jail; inventory = inventory;
    bankrupt = bankrupt; jail_turn_count = jtc;
  }

(* [get_cards_string cards] is the single-line representation 
   of all of the cards in the player's inventory *)
let rec get_cards_string = function
  | [] -> "" 
  | h :: [] ->
    Card.card_to_string h
  | h1 :: (h2 :: t2 as t) -> 
    (Card.card_to_string h1) ^ "|" ^ get_cards_string t

let player_to_string player = 
  player.name
  ^ ";" ^ (player.is_cpu |> string_of_bool)
  ^ ";" ^ (player.money |> string_of_int)
  ^ ";" ^ (player.position |> string_of_int)
  ^ ";" ^ (player.jail |> string_of_bool)
  ^ ";" ^ (get_cards_string player.inventory)
  ^ ";" ^ (player.bankrupt |> string_of_bool)
  ^ ";" ^ (player.jail_turn_count |> string_of_int)
