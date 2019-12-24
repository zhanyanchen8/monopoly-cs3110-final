(* Test plan:
   The majority of the testing done on our game was playtesting. This was
   done after we were able to verify the minimal components needed to run
   the game. To test these components, we write OUnit test cases. Int
   particular, those OUnit test cases cover the input of user commands
   that are output at the beginning of the game, the changing of money
   between players, resetting player positions, checking jail statuses,
   moving players around a gameboard, and ensuring that dice and players
   have been created within the state of the game. The modules that were
   tested by OUnit, therefore, are command.ml, player.ml, state.ml, and
   gameboard.ml. All of these tests were developed using black-box testing
   to test the overall functionality of the modules.

   Further interactions with the state of the game were tested with black
   box playtesting. In particular, we have a number of sav files that can
   be loaded into the game that tests specific elements of our game such.
   We set rng = false in order to facilitate the return of similar results
   and we also save turns so we can refer back to a specific instance of
   the game. The elements that are tested are self-explanatory based on the
   name of each sav file. Furthermore, we also tested other elements of our
   gameboard - such as functionality of specific groups of properties like
   railroads and utilities - by creating json files that represent a game
   board filled with all railroads, utilities, or a combination of the
   two (i.e. rail.json, utility.json, and chance_chest_board.json). 

   Our testing plan demonstrates the correctness of our system because at
   a lower level of our program, we use OUnit testing to ensure that the
   functionality of our program works before we move on to testing more
   complex factors. By playtesting, saving games, and saving turns of the
   games we play test, we are able to catch errors in our program, create
   game states where some error might pop up, and catch them to ensure
   that they are fixed.
*)

open OUnit2

(** [make_test name exp_out input] is the OUnit test formed by asserting the 
    quality of [exp_out] with [input]. *)
let make_test 
    (name : string) 
    (exp_out : 'b)
    (input : 'a) =
  name >:: (fun _ -> assert_equal exp_out input)

(** [make_1in_raises name excep input fn] is the OUnit test formed by asserting 
    that [fn input] raises [excep]. *)
let make_1in_raises
    (name : string)
    (excep : exn)
    (input : 'a) 
    (fn : 'a -> 'b) = 
  (* anonymous fn may not be necessary *)
  name >:: (fun _ -> assert_raises excep (fun _ -> fn input))  

(** [make_2in_raises name excep one two fn] is [make_1in_raises] with [fn] 
    taking two inputs [one] and [two] *)
let make_2in_raises 
    (name : string)
    (excep : exn)
    (one : 'a) 
    (two : 'b)
    (fn : 'a -> 'b -> 'c) = 
  name >:: (fun _ -> assert_raises excep (fun _ -> fn one two))

(*****************************************************************************)
open Command
let command_tests = 
  [
    (* Malformed tests *)
    make_1in_raises "parse empty" Malformed "" parse;
    make_1in_raises "parse only spaces" Malformed "     " parse; 

    (* Roll tests *)
    make_test "parse roll" Roll (parse "roll");
    make_test "parse r" Roll (parse "r");
    (* make_test "parse roll spaces" Roll (parse "  roll        "); *)

    (* Pstatus tests *)
    make_test "parse player status" Pstatus (parse "player status");
    (* make_test "parse player status spaces" Gstatus (parse "   player     status  "); *)
    make_test "parse pstatus" Pstatus (parse "pstatus");
    make_test "parse ps" Pstatus (parse "ps");
    make_test "parse my status" Pstatus (parse "my status");
    make_1in_raises "parse status" Malformed "status" parse;

    (* Gstatus tests *)
    make_test "parse game status" Gstatus (parse "game status");
    (* make_test "parse game status spaces" Gstatus (parse "   game     status  "); *)
    make_test "parse gstatus" Gstatus (parse "gstatus");

    (* Trade tests *)
    make_test "parse trade" Trade (parse "trade");
    make_test "parse t" Trade (parse "t");

    (* Quit tests *)
    make_test "parse quit" Quit (parse "quit");
    make_test "parse q" Quit (parse "q");

    (* Build tests *)
    make_test "parse build" Build (parse "build");
    make_test "parse b" Build (parse "b");

    (* Mortgage tests *)
    make_test "parse mortgage" Mortgage (parse "mortgage");
    make_test "parse m" Mortgage (parse "m");

    (* Unmortgage tests *)
    make_test "parse unmortgage" Unmortgage (parse "unmortgage");
    make_test "parse unm" Unmortgage (parse "unm");

    (* Help tests *)
    make_test "parse help" Help (parse "help");
    make_test "parse h" Help (parse "h");

    (* End tests *)
    make_test "parse end" End (parse "end");
    make_test "parse e" End (parse "e");
    make_test "parse end turn" End (parse "end turn");
  ]

(*****************************************************************************)
open Gameboard
let board8 = 
  Gameboard.from_json "board8.json" (Yojson.Basic.from_file "board8.json")
let board_full = 
  Gameboard.from_json "full_board.json" 
    (Yojson.Basic.from_file "full_board.json")

open Die
let d6 = create_die [1;2;3;4;5;6]
let std_dice = [d6;d6]

open Player
(** [cmp_lists f l1 l2] is true iff [l1] and [l2] have the same length and all 
    of their elements are equal according to comparison function [f]. 
    For best results, sort [l1] and [l2] first. *)
let rec cmp_lists f l1 l2 = 
  match l1, l2 with 
  | [], [] -> true
  | [], (_::_) | (_::_), [] -> false
  | h1::t1, h2::t2 -> f h1 h2 && cmp_lists f t1 t2

(** [card_equal c1 c2] is true iff all record values of [c1] are equal to 
    those of [c2]. *)
let card_equal c1 c2 = true

(** [player_equal p1 p2] is true iff all record values of [p1] are equal to 
    those of [p2]. *)
let player_equal p1 p2 = 
  get_name p1 = get_name p2
  && check_cpu p1 = check_cpu p2
  && get_money p1 = get_money p2
  && cmp_lists card_equal (get_inventory p1) (get_inventory p2)
  && get_position p1 = get_position p2
  && check_jail p1 = check_jail p2

let player_Ben = create_player "Ben" false 
let player_Ben' = player_Ben 
                  |> set_position 3
let player_Dubem = create_player "Dubem" false 
let player_Dubem' = player_Dubem 
                    |> set_position 6
let player_Yanchen = create_player "Yanchen" false 
let player_Yanchen' = player_Yanchen 
                      |> set_position 1
let player_Clarkson = create_player "God Himself" true 

let three_players = [(0, player_Ben); (1, player_Dubem); (2, player_Yanchen)]
let three_players' = 
  [(0, player_Ben'); (1, player_Dubem'); (2, player_Yanchen')]

(*****************************************************************************)
let player_tests = 
  [
    (* change_money tests *)
    make_test "Ben gets $200 :)" 
      1700 (player_Ben |> change_money 200 |> get_money);
    make_test "Dubem loses $1000 ;)" 
      500 (player_Dubem |> change_money ~-1000 |> get_money);
    make_test "Yanchen gets $100000 :o" 
      101500 (player_Yanchen |> change_money 100000 |> get_money);
    make_1in_raises "Dubem loses $2000 >:)" (Failure "projected negative money") 
      player_Dubem (change_money ~-2000);

    (* set_position tests *)
    make_test "no move" 0 (player_Ben |> get_position);
    make_test "moves to Go" 0 (player_Ben |> set_position 0 |> get_position);
    make_test "moves to 22" 22 (player_Ben |> set_position 22 |> get_position);
    make_2in_raises "moves to -1" 
      (Failure "Illegal move to a negative position") 
      ~-1 player_Ben set_position;
    make_test "moves to BIG" 
      1234567890 (player_Ben |> set_position 1234567890 |> get_position);

    make_test "moves to 13 to Go" 
      0 (player_Dubem |> set_position 13 |> set_position 0 |> get_position);
    make_test "moves to 22 to 24" 
      24 (player_Dubem |> set_position 22 |> set_position 24 |> get_position);
    make_2in_raises "moves to 11 to -1" 
      (Failure "Illegal move to a negative position") 
      ~-1 (player_Dubem |> set_position 11) set_position;
    make_test "moves to BIG 2x" 
      1234567890 (player_Dubem |> set_position 1234567890 
                  |> set_position 1234567890 |> get_position);

    (* toggle_jail tests *)
    make_test "into jail" true (player_Ben |> toggle_jail |> check_jail);
    make_test "in 'n outta jail" 
      false (player_Ben |> toggle_jail |> toggle_jail |> check_jail);
    make_test "in 'n out 'n into jail" 
      true (player_Dubem |> toggle_jail |> toggle_jail |> toggle_jail 
            |> check_jail);
    make_test "in 'n out 'n in 'n outta jail" 
      false (player_Yanchen |> toggle_jail |> toggle_jail |> toggle_jail 
             |> toggle_jail|> check_jail);

  ]

(*****************************************************************************)


open State

let gameboard_equal gb1 gb2 = 
  Gameboard.size gb1 = Gameboard.size gb2 &&
  let rec spaces_match t1 t2 = 
    match t1, t2 with
    | [], [] -> true
    | h1::t1, h2::t2 -> 
      if Gameboard.space_name h1 = Gameboard.space_name h2 
      then spaces_match t1 t2 else false
    | _ -> false
  in spaces_match (Gameboard.get_spaces gb1) (Gameboard.get_spaces gb2)

let die_equal d1 d2 = 
  let d1_sides = Die.sides d1 in 
  let d2_sides = Die.sides d2 in 
  let rec comp_sides sides1 sides2 = 
    match sides1, sides2 with 
    | [], [] -> true
    | h1::t1, h2::t2 ->
      if h1 = h2 then comp_sides t1 t2 else false
    | _ -> false 
  in comp_sides d1_sides d2_sides

let property_equal p1 p2 = 
  (Property.property_name p1) = (Property.property_name p2) &&
  (Property.group p1) = (Property.group p2) &&
  (Property.get_owner p1) = (Property.get_owner p2) &&
  (Property.rent_type p1) = (Property.rent_type p2)

let state_equal s1 s2 = 
  gameboard_equal (get_board s1) (get_board s2)
  && cmp_lists die_equal (fst (get_dice s1)) (fst (get_dice s2))
  && cmp_lists die_equal (snd (get_dice s1)) (snd (get_dice s2))
  && cmp_lists player_equal (get_players s1) (get_players s2)
  && cmp_lists property_equal (get_properties s1) (get_properties s2)
  && cmp_lists card_equal 
    (s1 |> get_board |> get_cards) (s2 |> get_board |> get_cards)
  && get_dcount s1 = get_dcount s2
  && speed_die_enabled s1 = speed_die_enabled s2
  && trading_enabled s1 = trading_enabled s2
  && get_can_end s1 = get_can_end s2
  && current_player s1 = current_player s2

(** [index_props ind st acc props] is [acc] where [acc] is all of the 
    properties in [props] owned by the current player in [st] paired with a 
    unique index [1 < ind < n] where [n] is the number of properties in [acc].
    [index_props] is not sorted. *)
let rec index_props ind st acc = function
  | [] -> acc
  | p :: t -> 
    if Property.get_owner p = current_player st 
    then let acc' = (ind, p) :: acc in 
      index_props (ind+1) st acc' t
    else index_props ind st acc t

let nospeed_notrade = (false,false)
let seed = Random.int (999999999)
let test_state_1 = 
  init_state three_players std_dice board_full nospeed_notrade "DUMMY" seed
let test_state_2 = 
  init_state three_players' std_dice board_full nospeed_notrade "DUMMY" seed
let test_state_mini = 
  init_state three_players std_dice board8 nospeed_notrade "DUMMY" seed
let move_Ben_3 = move 0 3 test_state_1
let move_Dubem_back_3 = move 1 ~-3 test_state_mini
let next_turn = step_turn test_state_1



(*****************************************************************************)
open Property
let props_8 = get_properties test_state_mini
let props_full = get_properties test_state_1

let ben_1 = set_position 1 player_Ben
let three_ben_on_1 = [(0, ben_1); (1, player_Dubem); (2, player_Yanchen)]
let ben_on_1 = 
  init_state three_ben_on_1 std_dice board8 nospeed_notrade "DUMMY" seed

(* maroon *)
let fst_st = get_prop_at 1 ben_on_1
(* sky *)
let fst_ave = get_prop_at 3 ben_on_1
let snd_ave = get_prop_at 4 ben_on_1

let ben_owns_sky = test_state_mini |> buy 0 fst_ave 1 |> buy 0 snd_ave 1
let ben_sky_owned = ben_owns_sky 
                    |> get_properties 
                    |> index_props 1 ben_owns_sky []
let ben_not_owns_sky = test_state_mini |> buy 0 fst_ave 1
let ben_not_sky_owned = ben_not_owns_sky 
                        |> get_properties 
                        |> index_props 1 ben_not_owns_sky []
let ben_owns_maroon = ben_on_1 |> buy 0 fst_st 1
let ben_maroon_owned = ben_owns_maroon
                       |> get_properties 
                       |> index_props 1 ben_owns_maroon []

(*****************************************************************************)
let state_tests = 
  [
    (*  (2) Player [p] has enough money to pay for [level] buildings on [prop]
        (3) [level] is higher than the current build state of [prop] and at most 5
        (5) [prop] belongs to a buildable group (i.e. color group)
        (6) Every poperty in the group of [prop] is in [owned], the indexed list 
            of properties owned by player [p] *)
    (* check_can_build tests *)
    make_test "correct current player" 0 (current_player ben_owns_maroon);
    make_test "correct num props 8" 8 
      (ben_owns_maroon |> get_properties |> List.length);
    make_test "correct property" "1st Street"
      (ben_owns_maroon |> get_prop_at 1 |> property_name);
    make_test "correct owner 0" 0 
      (ben_owns_maroon |> get_prop_at 1 |> get_owner);

    (* get_player tests: *)
    make_test "get first player" player_Ben (get_player 0 test_state_1);
    make_test "get third player" player_Yanchen (get_player 2 test_state_1);
    make_2in_raises "get -1 player" (Failure "player not found: -1") 
      ~-1 test_state_1 get_player;
    make_2in_raises "get fourth player" (Failure "player not found: 4") 
      4 test_state_1 get_player;
    make_2in_raises "get 10000000th player"
      (Failure "player not found: 10000000") 
      10000000 test_state_1 get_player;
    (* move tests *) 
    make_test "second player position" 
      0 (Player.get_position (get_player 1 move_Ben_3));
    make_test "first player after mvt" 
      3 (Player.get_position (get_player 0 move_Ben_3));
    make_test "move Dubem back 3"
      5 (Player.get_position (get_player 1 move_Dubem_back_3));
    (* roll tests *)

    (* get_dice tests *)
    make_test "test_state_1 dice" std_dice (snd (get_dice test_state_1));
    make_test "move_Ben_3 dice" std_dice (snd (get_dice move_Ben_3));
    (* current_player tests *)
    make_test "current player test_state_1" 0 (current_player test_state_1);
    make_test "current player move_Ben_3" 0 (current_player move_Ben_3);
    make_test "current player next_turn" 1 (current_player next_turn);
  ]
(*****************************************************************************)

let suite = 
  "testing... testing..."  >::: List.flatten [
    command_tests;   
    state_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite