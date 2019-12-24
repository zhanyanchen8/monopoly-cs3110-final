type speed = SInt of int | Bus | Man
type t = Normal of int * int list | Speed of int * speed list

let create_die lst = Normal (List.length lst, lst)

let create_speed_die lst = Speed (List.length lst, lst)

let rd (l,s) =  List.nth s (Random.int l)

let apply_d_normal d f msg = 
  match d with 
  | Normal (l,s) -> f (l,s) 
  | _ -> failwith msg

let apply_d_speed d f msg = 
  match d with 
  | Speed (l,s) -> f (l,s)
  | _ -> failwith msg

let roll_die d = apply_d_normal d rd "roll_die: not a normal die" 

let roll_speed_die d = apply_d_speed d rd "roll_speed_die: not a speed die"

let sides d = apply_d_normal d snd "sides: not a normal dies"

let speed_sides d = apply_d_speed d snd "speed_sides: not a speed die"

let size d = match d with 
  | Normal (l,_) | Speed (l,_) -> l

let speed_from_string = function
  | "Bus" -> Bus
  | "Man" -> Man
  | i -> begin match int_of_string_opt i with
      | Some i -> SInt i
      | _ -> failwith "speed_from_string: invalid input"
    end

let die_from_string s = 
  let s' = s |> String.split_on_char ',' in
  match s' with
  | "n" :: t -> create_die (List.map (fun x -> int_of_string x) t)
  | "s" :: t -> create_speed_die (List.map (fun x -> speed_from_string x) t)
  | _ -> failwith "die_from_string neither normal nor speed"

let speed_to_string = function
  | Bus -> "Bus"
  | Man -> "Man"
  | SInt i -> string_of_int i

let rec get_string sf = function 
  | [] -> ""
  | h :: [] -> sf h
  | h1 :: (h2 :: t2 as t) -> 
    (sf h1) ^ "," ^ get_string sf t

let die_to_string d = match d with 
  | Normal (_,s) -> "n," ^ get_string string_of_int s
  | Speed (_,s) -> "s," ^ get_string speed_to_string s
