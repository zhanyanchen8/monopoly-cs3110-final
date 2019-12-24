type rent = Gameboard.rent

type building = House | Hotel 

type t = {
  p_name : string;
  p_pos : int; 
  p_price : int;
  mortgaged : bool;
  p_rent_values : int list;
  p_owner : int;
  p_buildings : building list;
  p_group : Gameboard.group;
}

let init_property name pos price rents group = {
  p_name = name;
  p_pos = pos; 
  p_price = price;
  mortgaged = false;
  p_rent_values = rents;
  p_owner = ~-1;
  p_buildings = [];
  p_group = group;
}

let property_name prop =
  prop.p_name

let property_name_st prop = 
  let grp = "(" ^ (Gameboard.get_gg_name prop.p_group) ^ ")" in 
  let ext = 
    if prop.mortgaged
    then "(MRTG)" 
    else match prop.p_buildings with 
      | [] -> ""
      | [House] -> "(B)"
      | [House;House] -> "(BB)"
      | [House;House;House] -> "(BBB)"
      | [House;House;House;House] -> "(BBBB)"
      | [Hotel] -> "(HOTEL)"
      | _ -> failwith "invalid building configuration"
  in 
  String.concat " " [prop.p_name; grp; ext]

let board_ordinal prop =
  prop.p_pos

let get_price prop = 
  prop.p_price

let group prop =
  prop.p_group

let rent_type prop =
  Gameboard.get_gg_rent prop.p_group

(* figure out with group interactions and other rent values *)
let rent_values prop = 
  prop.p_rent_values

let house_cost prop =
  prop.p_group |> Gameboard.get_gg_build_cost

let get_owner prop =
  prop.p_owner

let set_owner player prop = {
  prop with p_owner = player
}

let buildings prop =
  prop.p_buildings

let set_buildings b prop = {
  prop with p_buildings = b;
}

let mortgaged prop =
  prop.mortgaged

let set_mortgage bool prop = {
  prop with mortgaged = bool;
}

let toggle_mortgage prop = {
  prop with mortgaged = not prop.mortgaged;
}

let deed prop = 
  let grp = prop.p_group in 
  if Gameboard.get_gg_buildable grp
  then begin 
    (prop.p_name |> String.uppercase_ascii) ^ " (" ^
    (grp |> Gameboard.get_gg_name) ^ ")\n"
    ^ "no houses:\t$" ^ ((List.nth prop.p_rent_values 0) |> string_of_int)^"\n"
    ^ "monopoly:\t$" ^ (2*(List.nth prop.p_rent_values 0)|>string_of_int)^"\n"
    ^ "1 house:\t$" ^ ((List.nth prop.p_rent_values 1) |> string_of_int) ^ "\n"
    ^ "2 houses:\t$" ^ ((List.nth prop.p_rent_values 2) |> string_of_int)^"\n"
    ^ "3 houses:\t$" ^ ((List.nth prop.p_rent_values 3) |> string_of_int)^"\n"
    ^ "4 houses:\t$" ^ ((List.nth prop.p_rent_values 4) |> string_of_int)^"\n"
    ^ "hotel:\t\t$" ^ ((List.nth prop.p_rent_values 5) |> string_of_int)^"\n"
    ^ "mortgage value: $" ^ ((prop.p_price / 2) |> string_of_int) ^ "\n"
    ^"building cost:  $"^(grp|>Gameboard.get_gg_build_cost|>string_of_int)^"\n"
  end
  else match Gameboard.get_gg_name grp with 
    | "rail" -> begin 
        (prop.p_name |> String.uppercase_ascii) ^ " (" ^
        (grp |> Gameboard.get_gg_name ) ^ ")" ^ "\n"
        ^ "1 railroad owned:\t$25\n"
        ^ "2 railroads owned:\t$50\n"
        ^ "3 railroads owned:\t$100\n"
        ^ "4 railroads owned:\t$200\n"
        ^ "mortgage value:\t\t$" ^ ((prop.p_price / 2) |> string_of_int) ^ "\n"
      end
    | "utility" -> begin 
        (prop.p_name |> String.uppercase_ascii) ^ " (" ^
        (grp |> Gameboard.get_gg_name ) ^ ")" ^ "\n"
        ^ "1 utility owned:\t4 × amount rolled\n"
        ^ "2 utilities owned:\t10 × amount rolled\n"
        ^ "mortgage value:\t\t$" ^ ((prop.p_price / 2) |> string_of_int) ^ "\n"
      end
    | _ as s -> s ^ " group unknown"

let buildings_from_string string = 
  let rec bfs = function 
    | [] -> []
    | h :: t -> 
      if h="house" then House :: bfs t
      else if h="hotel" then Hotel :: bfs t
      else failwith "invalid building string"
  in 
  match string with 
  | "" -> []
  | _ as s ->
    bfs (String.split_on_char ',' s)

let rents_from_string s = 
  s
  |> String.split_on_char ','
  |> List.map (fun x -> int_of_string x)

let get_rv = function
  | "" -> [] 
  | _ as s -> s |> rents_from_string

let rents_to_string lst = 
  let rec get_string = function 
    | [] -> ""
    | h :: [] ->
      (string_of_int h) 
    | h1 :: (h2 :: t2 as t) -> 
      (string_of_int h1) ^ "," ^ get_string t 
  in
  get_string lst 

let property_from_string s = 
  let name_head = String.split_on_char ';' s in 
  let name = List.hd name_head in 
  let pos_head = List.tl name_head in 
  let pos = (List.hd pos_head) |> int_of_string in 
  let price_head = List.tl pos_head in 
  let price = (List.hd price_head)|> int_of_string in 
  let mortgaged_head = List.tl price_head in 
  let mortgaged = (List.hd mortgaged_head) |> bool_of_string in 
  let rv_head = List.tl mortgaged_head in 
  let rv = get_rv (List.hd rv_head) in
  let owner_head = List.tl rv_head in 
  let owner = (List.hd owner_head) |> int_of_string in 
  let buildings_head = List.tl owner_head in 
  let buildings = (List.hd buildings_head) |> buildings_from_string in 
  let group_head = List.tl buildings_head in 
  let group = (List.hd group_head) |> Gameboard.group_from_string in 
  { p_name = name; p_pos = pos; p_price = price; 
    mortgaged = mortgaged; p_rent_values = rv; p_owner = owner;
    p_buildings = buildings; p_group = group }

let rec buildings_to_string = function
  | [] -> ""
  | h :: [] -> begin
      match h with 
      | House -> "house"
      | Hotel -> "hotel"
    end
  | h1 :: (h2 :: t2 as t) -> begin 
      match h1 with 
      | House -> "house" ^ "," ^ buildings_to_string t
      | Hotel -> "hotel" ^ "," ^ buildings_to_string t
    end

let property_to_string prop =
  prop.p_name
  ^ ";" ^ (prop.p_pos |> string_of_int)
  ^ ";" ^ (prop.p_price |> string_of_int)
  ^ ";" ^ (prop.mortgaged |> string_of_bool)
  ^ ";" ^ (prop.p_rent_values |> rents_to_string)
  ^ ";" ^ (prop.p_owner |> string_of_int)
  ^ ";" ^ (prop.p_buildings |> buildings_to_string)
  ^ ";" ^ (prop.p_group |> Gameboard.group_to_string)
