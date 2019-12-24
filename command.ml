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

exception Malformed

(** [parse s] is the string command that is associated with string [s] *)
let parse s = 
  match String.lowercase_ascii s with 
  | "roll" | "r" -> Roll
  | "player status" | "pstatus" | "ps" | "my status" -> Pstatus
  | "game status" | "gstatus" | "gs" -> Gstatus
  | "deed" | "d" -> Deed
  | "trade" | "t" -> Trade
  | "sell" | "s" -> Sell
  | "quit" | "q" -> Quit
  | "build" | "b" -> Build
  | "mortgage" | "m" -> Mortgage
  | "unmortgage" | "unm" | "u" -> Unmortgage
  | "help" | "h" -> Help
  | "end" | "e" | "end turn" -> End
  | "goto" | "gt" -> Goto
  | _ -> raise Malformed
