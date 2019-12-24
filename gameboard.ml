open Yojson.Basic.Util
open Card

type rent = RNone | Direct | Variable | Percentage

type card = {
  card_name: string;
  card_description: string;
  deck: string;
  functionality: string;
  amount: string;
}

type group = {
  gname: string;
  build: int;
  buildable: bool;
  rent_type: rent;
}

type space = {
  sp_name: string;
  position: int;
  group_name: string;
  price: int;
  rents: int list;
}

type t = {
  name: string;
  spaces: space list;
  groups: group list;
  jail_pos: int;
  go_to_jail_pos : int;
  cards: card list;
}

let to_rent = function
  | "Direct"
  | "direct" -> Direct
  | "Variable"
  | "variable" -> Variable
  | "Percentage"
  | "percentage" -> Percentage
  | _ -> RNone

let card_of_json json = {
  card_name = json |> member "name" |> to_string;
  card_description = json |> member "description" |> to_string;
  deck = json |> member "deck" |> to_string;
  functionality = json |> member "type" |> to_string;
  amount = json |> member "amount" |> to_string;
}

let group_of_json json = {
  gname = json |> member "groupname" |> to_string;
  build = json |> member "build" |> to_int;
  buildable = json |> member "buildable" |> to_bool;
  rent_type = json |> member "rent_type" |> to_string |> to_rent;
}

let space_of_json json = {
  sp_name = json |> member "spacename" |> to_string;
  position = json |> member "position" |> to_int;
  group_name = json |> member "group" |> to_string;
  price = json |> member "price" |> to_int;
  rents = json |> member "rents" |> to_list |> List.map to_int;
}

let assign_board name json = {
  name = name;
  spaces = json |> member "spaces" |> to_list |> List.map space_of_json;
  groups = json |> member "groups" |> to_list |> List.map group_of_json;
  jail_pos = json |> member "jail_pos" |> to_int;
  go_to_jail_pos  = json |> member "go_to_jail_pos" |> to_int;
  cards = json |> member "cards" |> to_list |> List.map card_of_json;
}

let from_json name json = 
  assign_board name json

let get_gb_name gb = 
  gb.name

let size gb = 
  let rec size' acc = function
    | [] -> acc
    | h :: t -> size' (1 + acc) t 
  in 
  size' 0 gb.spaces

(** [modded x m] is a wrap-around resetting values to its
    new position on the game board.
    Requires: [m] > 0. *)
let rec modded x m = 
  if x<0 then modded (x+m) m 
  else if x>=m then modded (x-m) m
  else x

let get_space pos gb = 
  let rec get_space' pos = function 
    | [] -> failwith "Space not found"
    | h :: t -> 
      if h.position=pos then h else get_space' pos t 
  in 
  get_space' (modded pos (size gb)) gb.spaces

let get_position space gb = 
  let rec get_position' space = function 
    | [] -> failwith "Invalid space"
    | h :: t -> 
      if h=space then h.position else get_position' space t
  in 
  get_position' space gb.spaces

let jail_pos gb =
  gb.jail_pos

let gtj_position gb = 
  gb.go_to_jail_pos

let space_name sp = 
  sp.sp_name

let space_pos sp = 
  sp.position

let space_price sp = 
  sp.price

let space_group_name sp = 
  sp.group_name

let get_spaces gb = 
  gb.spaces

let get_card_name c = 
  c.card_name

let get_card_description c = 
  c.card_description

let get_card_deck c = 
  c.deck

let get_card_functionality c =
  c.functionality

let get_card_amount c = 
  c.amount

let get_cards gb = 
  gb.cards

let get_groups gb = 
  gb.groups

(** [find_group nm gs] is the group in the list [gs] with 
    the name [nm] *)
let rec find_group nm = function
  | [] -> failwith "group not there"
  | h :: t -> 
    if h.gname=nm then h else find_group nm t

let get_group_w_name nm gb = 
  let rec group_helper nm = function
    | [] -> failwith "name not here"
    | h :: t ->
      if h.group_name=nm
      then find_group h.group_name gb.groups
      else group_helper nm t in 
  group_helper nm gb.spaces

let get_rents sp = 
  sp.rents

let get_gg_name gg = 
  gg.gname

let get_gg_build_cost gg = 
  gg.build

let get_gg_buildable gg = 
  gg.buildable

let get_gg_rent gg = 
  match gg.rent_type with 
  | Direct -> "direct"
  | RNone -> "none"
  | Variable -> "variable"
  | Percentage -> "percentage"

let rent_from_string = function 
  | "rnone" -> RNone
  | "direct" -> Direct
  | "variable" -> Variable
  | "percentage" -> Percentage
  | _ -> failwith "invalid rent string"

let group_from_string s = 
  let name_head = String.split_on_char ',' s in 
  let name = List.hd name_head in 
  let build_head = List.tl name_head in 
  let build = (List.hd build_head) |> int_of_string in 
  let buildable_head = List.tl build_head in 
  let buildable = (List.hd buildable_head) |> bool_of_string in 
  let rent_head = List.tl buildable_head in 
  let rent = (List.hd rent_head) |> rent_from_string in 
  { gname = name; build = build; 
    buildable = buildable; rent_type = rent }

let rent_to_string = function 
  | RNone -> "rnone"
  | Direct -> "direct"
  | Variable -> "variable"
  | Percentage  -> "percentage"


let group_to_string gg = 
  gg.gname
  ^ "," ^ (gg.build |> string_of_int)
  ^ "," ^ (gg.buildable |> string_of_bool)
  ^ "," ^ (gg.rent_type |> rent_to_string)
