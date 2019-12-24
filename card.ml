open Yojson.Basic.Util

type amount_type = 
  | Integer of int
  | Tuple of int * int 
  | Group of string

type card_type =
  | None
  | Raise
  | Lose
  | MoveTo
  | MovetoJail
  | Repair
  | LoseEachPlayer
  | EarnEachPlayer
  | MoveDist
  | GetOut

type card_deck =
  | DNone
  | Chance
  | CommunityChest

type card_location =
  | Deck
  | Player of int

type t = {
  name: string;
  func: card_type; 
  deck: card_deck;
  location: card_location;
  description: string;
  amount: amount_type;
}

(* [tuple_of_string str] is a tuple representing 
    outputs for house/hote repairs*)
let tuple_of_string str = 
  let loc = String.index str ',' in 
  let int_1 = String.sub str 1 2 in 
  let int_2 = String.sub str (loc+2) 3 in 
  Tuple (int_of_string int_1, int_of_string int_2)

(* [set_card_amt amt] is the associated change with a card *)
let set_card_amt amt = 
  match String.get amt 0 with 
  |'1' |'2' |'3' |'4' |'5' 
  |'6' |'7' |'8' |'9' -> Integer (int_of_string amt)
  | '[' -> tuple_of_string amt
  | _ -> Group amt

(* [set_card_func f] is a functionality based on [f]*)
let set_card_func = function 
  | "moveto" -> MoveTo
  | "earn" -> Raise
  | "get out" -> GetOut
  | "movedist" -> MoveDist
  | "movetojail" -> MovetoJail
  | "repairs" -> Repair
  | "spend-each-player" -> LoseEachPlayer
  | "spend" -> Lose
  | "earn-each-player" -> EarnEachPlayer
  | _ -> None

(* [set_deck_type d] is a deck type based on [d]*)
let set_deck_type = function 
  | "Chance" -> Chance 
  | "Community Chest" -> CommunityChest
  | _ -> DNone

let init_card n desc d f a = 
  {
    name = n;
    func = set_card_func f;
    deck = set_deck_type d;
    location = Deck;
    description = desc;
    amount = set_card_amt a;
  }

let get_card_name card = 
  card.name

let get_card_description card = 
  card.description

let get_card_amount card = 
  card.amount

let get_card_type card = 
  match card.func with 
  | Raise -> "raise"
  | Lose -> "lose"
  | MoveTo -> "moveto"
  | MovetoJail -> "movetojail"
  | Repair -> "repair"
  | LoseEachPlayer -> "lose_player"
  | EarnEachPlayer -> "earn_player"
  | MoveDist -> "move_dist"
  | GetOut -> "get_out_jail"
  | _ -> "other"

let get_card_deck card = 
  match card.deck with 
  | DNone -> ""
  | Chance -> "chance"
  | CommunityChest -> "chest"

let create_deck card = 
  failwith "wait, what?"

let shuffle cards =
  let rec shuffle' acc lst =
    let len = List.length lst in
    if len = 0
    then acc
    else let temp = List.nth lst (Random.int len) in
      let filt = List.filter (fun x -> x<>temp) lst in
      shuffle' (temp::acc) filt
  in
  shuffle' [] cards

let replace_deck cards = 
  failwith "replace_deck unimplemented"

let change_location card = 
  failwith "change_location unimplemented"

let amount_type_to_string = function
  | Integer x -> ("i" ^ (string_of_int x))
  | Tuple (i1,i2) -> 
    "t" ^ (string_of_int i1) ^ "+" ^ (string_of_int i2)
  | Group s -> s

let card_type_to_string = function
  | Raise -> "raise"
  | Lose -> "lose"
  | MoveTo -> "moveto"
  | MovetoJail -> "movetojail"
  | Repair -> "repair"
  | LoseEachPlayer -> "loseeachplayer"
  | EarnEachPlayer -> "earneachplayer"
  | MoveDist -> "movedist"
  | GetOut -> "getout"
  | _ -> failwith "None naming issue"

let card_deck_to_string = function
  | DNone -> "none"
  | Chance -> "chance"
  | CommunityChest -> "communitychest"

let card_location_to_string = function
  | Deck -> "deck"
  | Player i -> string_of_int i

(* these will have an initial character
   i, t, or g
   so we know what to do with each.
   the pair is separated by a tilde ~ *)
let amount_type_from_string s = 
  print_endline ("amt type: " ^ s); (* TODO remove this print *)
  match s.[0] with 
  | 'i' -> Integer (int_of_string (String.sub s 1 (~-1 + String.length s)))
  | 't' -> begin
      let tupl = String.sub s 1 (~-1 + String.length s) in 
      let seg = String.split_on_char '+' tupl in 
      Tuple ((seg |> List.hd |> int_of_string),
             (seg |> List.tl |> List.hd |> int_of_string))
    end
  | 'g' -> Group (String.sub s 1 (~-1 + String.length s))
  | _ -> Integer 0

let card_type_from_string = function
  | "none" -> failwith "None naming issue"
  | "raise" -> Raise
  | "lose" -> Lose
  | "moveto" -> MoveTo
  | "movetojail" -> MovetoJail
  | "repair" -> Repair
  | "loseeachplayer" -> LoseEachPlayer
  | "earneachplayer" -> EarnEachPlayer
  | "movedist" -> MoveDist
  | "getout" -> GetOut
  | _ -> failwith "invalid card_type string"

let card_deck_from_string = function
  | "none" -> failwith "None naming issue"
  | "chance" -> Chance 
  | "communitychest" -> CommunityChest
  | _ -> failwith "invalid card_deck string"

let card_location_from_string = function 
  | "deck" -> Deck 
  | _ as s -> Player (int_of_string s)

let card_from_string s = 
  let name_head = String.split_on_char '~' s in 
  let name = List.hd name_head in 
  let func_head = List.tl name_head in 
  let func = List.hd func_head |> card_type_from_string in 
  let deck_head = List.tl func_head in 
  let deck = List.hd deck_head |> card_deck_from_string in 
  let loc_head = List.tl deck_head in 
  let loc = List.hd loc_head |> card_location_from_string in 
  let desc_head = List.tl loc_head in 
  let desc = List.hd desc_head in 
  let amt_head = List.tl desc_head  in 
  let amt = List.hd amt_head |> amount_type_from_string in 
  {
    name = name; func = func; deck = deck; 
    location = loc; description = desc; amount = amt;
  }

let card_to_string card =
  card.name 
  ^ "~" ^ (card.func |> card_type_to_string)
  ^ "~" ^ (card.deck |> card_deck_to_string)
  ^ "~" ^ (card.location |> card_location_to_string)
  ^ "~" ^ (card.description)
  ^ "~" ^ (card.amount |> amount_type_to_string)