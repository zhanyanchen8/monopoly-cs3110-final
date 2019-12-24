open Unix
open State
open Command
open Gameboard
open Player

(* [save_turns] is if we want to save each turn of the game. 
    This is primarily used for play-testing purposes. *)
let save_turns = true

(* [valid_command str] is if [str] is a valid command. *)
let valid_command str = 
  match Command.parse str with 
  | exception Malformed -> false
  | _ -> true

(** [int_of_string_alph s] is the string represented by string [s]
    returns 0 if string is not an integer representation *)
let int_of_string_alph s = 
  try int_of_string s with Failure _ -> 0

(** [jail_turn state] is [state] after [current_player state] has attempted to 
    get out of jail by rolling, paying $50, or playing a 
    "Get Out of Jail Free" card. 
    If the player is not in jail, [jail_turn state] is [state]. *)
let rec jail_turn state jail_choice =  
  let curr = State.current_player state in 
  match jail_choice with 
  | "1" -> State.roll state
  | "2" -> begin
      state |> reset_jtc curr |> State.pay_out_jail curr   
    end
  | "3" -> state |> State.use curr
  | "4" -> let state' = setup_mortgage state in jail_turn_input state'
  | "5" -> let state' = prompt_build_sell_input false state in 
    jail_turn_input state'
  | "6" -> print_endline (game_status state); jail_turn_input state
  | _ -> failwith "jail_turn jail_choice out of range - check call to prompt"

and jail_turn_input state = 
  let curr = state |> current_player in 
  let player = state |> get_player curr in
  if Player.get_jail_turn_count player > 3
  then begin 
    print_endline "You can no longer attempt to roll out of jail";
    State.pay_out_jail curr state
  end
  else if State.get_dcount state = 0 then begin 
    print_endline 
      (String.concat "\n" 
         ["turn in jail: " ^ (player |> get_jail_turn_count |> string_of_int);
          "You are in jail!\nWhat would you like to do?";
          "FUNDS: $" ^ (player |> Player.get_money |> string_of_int);
          "1. Roll"; "2. Pay $50"; "3. Use get out of jail free card";
          "4. Mortgage"; "5. Sell"; "6. Game Status";]);
    let jail_choice =
      prompt (fun x -> match int_of_string_opt x with
          | Some i when i >= 1 && i <= 6 -> true
          | _ -> false) "out of range\n" in 
    jail_turn state jail_choice
  end
  else begin clear (); toggle_can_end state end

(* [list_owned prop_assoc] is the prompt string for selecting a property *)
let rec list_owned = function
  | [] -> "" 
  | (k,v) :: t -> 
    (string_of_int k) ^ ". "
    ^ (Property.property_name_st v)
    ^ "\n" ^ list_owned t

(** [get_deed st] propmts the player to select a property and then returns
    its deed *)
let get_deed st = 
  clear ();
  let props = prop_assoc_of
      (fun x -> match (x |> Property.group |> Gameboard.get_gg_name) with
         | "go" | "free" | "chance" | "chest" | "tax" -> false 
         | _ -> true) st in 
  print_endline "select a property"; print_endline (list_owned props);
  let choice = prompt (fun x ->
      x = "q" || x = "e" || x = "d" || 
      (try (int_of_string x) >= 0
           && (int_of_string x) <= (List.length props)
       with Failure _ -> false)) "invalid\n"
               |> int_of_string_alph in 
  if choice = 0 then "" else 
    let prop = List.assoc choice props in
    (Property.deed prop) ^ "Owner:\t\t"
    ^ begin match Property.get_owner prop with
      | -1 -> "NONE"
      | _ as i -> st |> get_player i |> Player.get_name 
    end ^ "\n"

(** [get_filenames ext] is the string list of files availible whose names
    end with the string [ext] *)
let get_filenames ext = 
  let handle = opendir "." in 
  let rec get_names acc = 
    try
      let filename = readdir handle in 
      let len = String.length ext in 
      if String.length filename > len
      && String.compare
           (String.sub filename ((String.length filename) - len) len)
           ext = 0
      then get_names (filename::acc)
      else get_names acc
    with End_of_file -> acc in 
  [] |> get_names |> List.sort String.compare

(** [save_game state] saves the game to its respective file *)
let save_game state = begin
  clear ();
  print_endline "would you like to save? (y/n)";
  let save_choice = begin
    match prompt (fun x -> x="y" || x="n") "invalid\n" with 
    | "y" -> true
    | "n" -> false
    | _ -> failwith "never happens"
  end in 
  if save_choice
  then begin 
    state_to_file state; print_string "saved to ";
    print_string (get_filename state); print_endline ".sav"
  end
  else if List.mem ((get_filename state) ^ ".sav") (get_filenames ".sav")
  then ()
  else ("rm -r " ^ (get_filename state)) |> Sys.command |> ignore;
  stall 0.5; print_endline "Thank you for playing!";
end

(* [run_game state] processes a user's input and manipulates [state]
    based on the actions the user asks for. *)
let rec run_game state = 
  if e_winner state
  then state |> toggle_game_over |> victory
  else
    let tc_string = state |> get_turn_count |> string_of_int in 
    let tc_string' = parse_input tc_string in 
    print_endline ("Turn " ^ tc_string');
    let curr = current_player state in
    let player = get_player curr state in
    let current_name = Player.get_name player in 
    print_string current_name;
    if current_name.[String.length current_name - 1] = 's' then
      print_string "'" else print_string "'s"; print_string " turn";
    if get_can_end state then print_char '\n' else print_endline "*";
    if Player.check_cpu player then cpu_turn state
    else if not (get_can_end state) && Player.check_jail player
    then state |> jail_turn_input |> run_game
    else parse_command current_name state

and parse_command current_name state = 
  match (prompt valid_command 
           "Invalid command. Type \"help\" for a list of valid commands\n")
        |> parse with
  | Roll -> if get_can_end state
    then begin print_endline ": cannot roll\n"; run_game state end
    else begin print_newline(); state |> roll_dice current_name |> run_game end
  | Pstatus -> print_endline (player_status (current_player state) state); 
    run_game state
  | Gstatus -> print_endline (game_status state); run_game state
  | Deed -> print_endline (get_deed state); run_game state
  | Trade -> if trading_enabled state then run_game (State.setup_trade state)
    else begin print_endline "trading disabled"; run_game state end
  | Sell -> state |> prompt_build_sell_input false |> run_game
  | End -> if State.get_can_end state then end_turn state save_turns
    else begin print_endline "cannot end\n"; run_game state end
  | Build -> state |> prompt_build_sell_input true |> run_game 
  | Mortgage -> state |> setup_mortgage |> run_game
  | Unmortgage -> state |> setup_unmortgage |> run_game
  | Help -> print_help (); run_game state
  | Quit -> save_game state | Goto -> setup_turn state

and end_turn state save_turns = begin 
  print_endline "\nturn ended\n"; 
  clear ();
  if save_turns
  then state |> State.step_turn |> State.save_turn
  else ();
  let next_p = State.next_player state in
  let next_pl = state |> State.get_player next_p in
  if Player.check_jail next_pl
  then state |> inc_jtc next_p |> State.step_turn |> run_game
  else state |> State.step_turn |> run_game 
end

and print_help _ = 
  print_endline (String.concat "\n"
                   ["Valid commands are:";
                    "(r)oll - roll and move";
                    "(p)layer (s)tatus - see player / (your) status";
                    "(g)ame (s)tatus - see game status";
                    "(d)eed - view the deed of a property";
                    "(t)rade - begin a trade";
                    "(s)ell - sell a house or a hotel";
                    "(e)nd - end your turn";
                    "(b)uild - build houses or hotel";
                    "(q)uit - quit and choose to save the game";
                    "(m)ortgage - mortgage a property";
                    "(u)ortgage - unmortgage a mortgaged property";
                    "(h)elp - help"]);

and parse_input tc_string = 
  match String.length tc_string with 
  | 1 -> "000" ^ tc_string
  | 2 -> "00" ^ tc_string
  | 3 -> "0" ^ tc_string
  | 4 -> tc_string
  | _ -> failwith "over 9999 turns have passed"

and cpu_turn state = 
  stall 0.5; if State.get_can_end state 
  then begin
    print_endline "turn ended\n";
    if save_turns then state |> State.step_turn |> State.save_turn else ();
    state |> step_turn |> run_game end
  else begin
    let player = (state |> current_player |> get_player) state in
    if Player.check_jail (player)
    then let choice = match Player.get_inventory player with
        | h::_ -> 3
        | _ ->
          let total_props = List.length (get_properties state) in
          if get_properties_bought state < total_props*4/5 then 1
          else if Player.get_money player > 300 then 2 else 1 in
      buffer_cpu ("Choice: " ^ string_of_int choice);
      run_game (choice |> string_of_int |> jail_turn state)
    else let new_st' = roll_dice (Player.get_name player) state in 
      print_char '\n'; run_game new_st';
  end

and setup_turn st = 
  clear ();
  let ct = st |> get_turn_count in
  print_endline ("Current turn: " ^ (ct |> string_of_int));
  print_endline ("Which turn would you like to go back to? 
    (enter 0 to go back)");
  let choice = begin prompt
      (fun x ->
         let x' = int_of_string_alph x in 
         (x = "q" || x = "e" || x = "d" ||
          (x' >= 0 && x' <= ct))
      ) "invalid\n"
  end |> int_of_string_alph in 
  if choice=0 then begin 
    clear ();
    run_game st
  end
  else run_game (State.load_turn choice (get_filename st))

(** [victory state] displays the victory sequence for the winning player *)
and victory state = 
  let curr = current_player state in 
  let player = get_player curr state in 
  let name = Player.get_name player in 
  print_endline (name ^ " wins!");
  print_endline (player_status curr state); 
  press_enter ();
  clear ();
  print_endline ("Would you like to undo turns? (y/n)");
  let gt_choice = begin
    match prompt (fun x -> x="y" || x="n") "invalid\n" with 
    | "y" -> true
    | "n" -> false
    | _ -> failwith "never happens"
  end in 
  if gt_choice 
  then setup_turn state
  else save_game state

(** [valid_session_name name] is true if the string [name] is not 
    the name of an existing play session *)
let valid_session_name (name:string) = 
  (Str.(string_match (regexp "^[a-zA-Z0-9]+$") name 0))
  && not (List.mem (name ^ ".sav") (get_filenames ".sav"))

(** [list_boards names] is the display that the player uses to select the
    game board using the strings of [names] *)
let list_boards names =
  let rec list_boards' ord = function 
    | [] -> "" 
    | h :: t -> 
      (string_of_int ord) 
      ^ ". " ^ h ^ "\n" 
      ^ (list_boards' (ord+1) t) in 
  list_boards' 1 (names)

(** [list_players players] prints the ordinal number of every player
    followed by their name, and cpu status*)
let rec list_players = function
  | [] -> ()
  | h :: t -> 
    print_int (1 + fst h); print_string "  ";
    if Player.check_cpu (snd h) 
    then print_string "CPU" 
    else print_string "HMN";
    print_string ("  " ^ Player.get_name (snd h));
    print_char '\n'; list_players t

(* [valid_name name players] is true iff [name] is not already a name in the
   list of existing players, and if [name] is not solely whitespace *)
let rec valid_name name = function 
  | [] ->
    (Str.(string_match (regexp "^[a-zA-Z0-9]+$") name 0))
    && (String.length name < 40)
  | h :: t -> if name = (Player.get_name (snd h))
    then false
    else valid_name name t

(** [make_cpus acc pc cpuc] promts the user to assign names and genders 
    to all of the cpu players. [pc] is the number of remaining cpu players
    to assign, and [cpuc] is the total amount of cpu players in the game *)
let rec make_cpus acc pc cpuc = 
  if pc=0
  then begin print_char '\n';
    list_players (List.rev acc); acc
  end
  else let pname = 
         begin print_string "\nenter name of cpu ";
           print_int (cpuc - pc + 1); print_char '\n';
           prompt (fun x -> valid_name x acc)
             "invalid\n" end in
    make_cpus (((List.length acc),
                (Player.create_player pname true)) :: acc) 
      (pc-1) cpuc

(* [make_players acc pc hc] prompts the user to specify the names and genders
   of each player. [pc] is the total number of players, and [hc] is the total
   number of human players 
   Requires: [pc >= hc] *)
let rec make_players acc pc hc = 
  if hc=0 then (make_cpus acc pc pc)
  else let pname = begin 
      print_string "\nenter name of player "; 
      print_int (List.length acc + 1); print_char '\n';
      prompt (fun x -> valid_name x acc) "invalid\n" 
    end in
    make_players (((List.length acc),
                   (Player.create_player pname false)) :: acc) 
      (pc-1) (hc-1)

(* [new_game ()] creates a new Monopoly game. *)
let rec new_game () = 
  print_endline "\nhow many total players? (2-8)";
  let playercount = prompt (fun x ->
      try (let num = int_of_string x in
           num<=8 && num>=2) with
      | Failure _ -> false ) "out of range\n" |> int_of_string in 
  print_string "\nhow many of these "; print_int playercount;
  print_endline " players are human players?";
  let humancount =  prompt (fun x ->
      try (let num = int_of_string x in
           num<=playercount && num>=0) with
      | Failure _ -> false ) "out of range\n" |> int_of_string in 
  print_string "this game will have "; print_int humancount;
  if humancount=1 then print_string " human player and "
  else print_string " human players and ";
  let cnt = playercount - humancount in print_int cnt;
  if cnt=1 then print_endline " CPU player" else print_endline " CPU players";
  new_game' playercount humancount

and new_game' playercount humancount = 
  print_endline "\nspeed die? (y/n)";
  let speed_die_on = match prompt (fun x -> x="y" || x="n") "invalid\n" with 
    | "y" -> true | "n" -> false
    | _ -> failwith "new_game speed_die_on out of range - check call to prompt"
  in 
  if speed_die_on then print_endline "speed die enabled" 
  else print_endline "speed die disabled"; print_endline "\ntrading? (y/n)";
  let trading_on =
    match prompt (fun x -> x="y" || x="n") "invalid\n" with 
    | "y" -> true | "n" -> false
    | _ -> failwith "new_game trading_on out of range" in 
  if trading_on then print_endline "trading enabled" 
  else print_endline "trading disabled";
  stall 0.5; clear ();
  new_game'' playercount humancount speed_die_on trading_on

and new_game'' playercount humancount speed_die_on trading_on =
  let players = make_players [] playercount humancount in
  let d6 = Die.create_die [1;2;3;4;5;6] in
  let std_speed = 
    Die.create_speed_die [SInt 0; SInt 1; SInt 2; SInt 3; Bus; Man] in
  let dice = match speed_die_on with 
    | true -> [d6; d6; std_speed] | false -> [d6; d6] in
  let board_names = get_filenames ".json" in 
  let num_boards = List.length board_names in 
  print_endline "\nselect a monopoly board";
  print_endline (list_boards board_names);
  let board_num = prompt (fun x -> try (let num = int_of_string x in
  num >= 1 && num <= num_boards) with | Failure _ -> false ) "invalid\n" in
  let board_name = List.nth (board_names) (~-1 + int_of_string board_num) in
  let gb = Gameboard.from_json board_name(Yojson.Basic.from_file board_name) in
  let flags = (speed_die_on, trading_on) in
  print_endline "\ngive a name to this session";
  let fn = prompt (fun x -> valid_session_name x) "invalid\n" in 
  let initial_state = State.init_state players dice gb flags fn 
      (() |> Unix.time |> Float.to_int) in 
  clear (); State.save_turn initial_state; run_game initial_state

(* [main ()] is the the initial player prompt.
   It either goes to new game or load game *)
let rec main () = 
  print_endline "welcome to Monopoly!";
  print_endline "1. new game";
  print_endline "2. load save";
  print_endline "3. delete save";
  let choice = begin
    prompt (fun x ->
        try 
          let num = int_of_string x in
          num=1 || num=2 || num=3 
        with Failure _ -> false ) "invalid\n" 
  end in 
  match choice with 
  | "1" -> new_game ()
  | "2" -> load_game ()
  | "3" -> delete_save ()
  | _ -> failwith "main () choice out of range - check call to prompt"

(* prompts player to enter the name of a previously saved
   game file *)
and load_game () = 
  print_endline "\nselect save file";
  let save_names = get_filenames ".sav" in 
  if (List.length save_names) = 0
  then begin buffer "no saves"; main () end
  else let num_saves = List.length save_names in 
    print_endline (list_boards save_names);
    let save_num = begin 
      prompt (fun x -> try 
                 let num = int_of_string x in
                 num >= 0 && num <= num_saves
               with Failure _ -> false ) "invalid\n" 
    end in
    if save_num="0" then main () else
      let save_name = List.nth (save_names) (~-1 + int_of_string (save_num)) in 
      try let loaded_state = State.state_from_file save_name in
        clear (); run_game loaded_state 
      with
      | Invalid_save s -> begin buffer s; main () end
      | End_of_file -> begin buffer "invalid .sav file"; main () end 

(* prompts player to delete a previously saved save file *)
and delete_save () = 
  print_endline "\nselect save file";
  let save_names = get_filenames ".sav" in 
  if List.length save_names = 0
  then begin buffer "no saves"; main () end
  else
    let num_saves = List.length save_names in 
    print_endline (list_boards save_names);
    let save_num = begin 
      prompt (fun x -> try 
                 let num = int_of_string x in
                 num >= 0 && num <= num_saves
               with Failure _ -> false ) "invalid\n" 
    end in 
    if save_num="0" then main () else
      let save_name = List.nth (save_names) (~-1 + int_of_string (save_num)) in 
      ("rm " ^ save_name) |> Sys.command |> ignore;
      ("rm -r " ^ (String.sub save_name 0 (~-4 + String.length save_name)))
      |> Sys.command |> ignore;
      buffer (save_name ^ " deleted"); main ()

let () = main ()
