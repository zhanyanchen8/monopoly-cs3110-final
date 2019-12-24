(** [command] represents the result of processing player input in the form of a 
    command to change the game state. *)

(** The type representing the possible types of commands
    users might request during game play. *)
type command =
  | Roll 
  | Pstatus
  | Gstatus
  | Deed
  | Trade 
  | Sell
  | End
  | Build 
  | Mortgage 
  | Unmortgage
  | Help
  | Quit
  | Goto

(** Raised when a command is Malformed (i.e. invalid). *)
exception Malformed

(** [parse s] parses [s] into a [command].
    Requires: [s] containes only lowercase letter characters (a-z).
    Raises: [Malformed] if the command is invalid. A command is malformed if it 
    does not match one of the valid commands. *)
val parse : string -> command