(** This module describes the functionality of any die in 
    the game, whether it be a regular die or a speed die. *)

(** representation of a speed die *)
type speed = SInt of int | Bus | Man

(** representation of a die's sides and how many sides the die has *)
type t = Normal of int * int list | Speed of int * speed list

(** [create_die lst] is the die with sides being the elements of [lst] *)
val create_die : int list -> t

(** [create_speed_die lst] is the die with sides being the elements of [lst] *)
val create_speed_die : speed list -> t

(** [roll d] is an int representing the value rolled from [d]. *)
val roll_die : t -> int

(** [roll_speed_die d] is a speed representing the value rolled on speed 
    die [d] *)
val roll_speed_die : t -> speed

(** [sides d] is a list representation of the sides of [d]. *)
val sides : t -> int list

(** [speed_sides d] is a list representation of the sides of [d] *)
val speed_sides : t -> speed list

(** [size d] is the number of sides on die [d] *)
val size : t -> int

(** [die_from_string s] is a die created from its string representation *)
val die_from_string : string -> t

(** [die_to_string t] is the string representation
    of die [t] *)
val die_to_string : t -> string