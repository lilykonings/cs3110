open Definitions
open Constants

(********************************************************************************
 * Hashtable with key = string                                                  *
 ********************************************************************************)

module Sstring : sig
  type t = string
  val hash : t -> int
  val equal : t -> t -> bool
end

module Table : Hashtbl.S with type key = Sstring.t

val hash_to_list : 'a Table.t -> 'a list

(********************************************************************************
 * Calculation functions (Important!)                                           *
 ********************************************************************************)

(* Returns the multiplier for a stat at a specified stage.
 *
 * Precondition: The stage is in the range [-6,6]. *)
val multiplier_of_modifier : int -> float

(* Returns the damage done by an attack, given the attacker's Atk, the defender's
 * Def, the move's power, and a multiplier, in that order. If the attack's 
 * Steamtype is special, use SpA instead of Atk and SpD instead of Def. See the 
 * writeup for details about the multiplier argument. *)
val calculate_damage : int -> int -> int -> float -> int 

(* weakness type1 type2 returns the effectiveness of type1 (the attacking type)
 * against type2 (the defending type). *)
val weakness : steamtype -> steamtype -> effectiveness

(* Returns to multiplier that corresponds to the input effectiveness. *)
val multiplier_of_effectiveness : effectiveness -> float

(* calculate_type_matchup move_type (mon_type1, mon_type2) returns the overall
 * effectiveness and multiplier of using a move that has the element move_type
 * on a Steammon whose types are mon_type1 and mon_type2. *)
val calculate_type_matchup : 
	steamtype -> steamtype option * steamtype option -> effectiveness * float

(********************************************************************************
 * String conversion and file parsing functions                                 *
 ********************************************************************************)

val type_of_string : string -> steamtype
val stat_of_string : string -> stat
val item_of_string : string -> item
val string_of_color : color -> string

val string_of_type : steamtype -> string
val string_of_stat : stat -> string
val status_of_string : string -> status
val string_of_status : status -> string
val string_of_effect : effect -> string
val string_of_effect_result : effect_result -> string
val string_of_target : target -> string
val string_of_item : item -> string

(* wordify delim str turns str into a list of words, where the divisions
 * between words are determined by the delimiter delim. *)
val wordify : char -> string -> string list

(* read_lines filename reads in all the lines from a file with the name
 * filenames, and returns a list. Each element of the list contains one
 * line from the file.
 *
 * Note: This function ignores all lines that begin with '#'.
 * Precondition: filename is a valid file name.*)
val read_lines : string -> string list

(********************************************************************************
 * Helper functions (for your convenience)                                      *
 ********************************************************************************)

(* Returns the color of the opponent. *)
val invert_color : color -> color

(* Returns true if the input Steamtype is special. *)
val is_special : steamtype -> bool

(* calc_attackers_attack attacker att returns attacker's Atk if move is a 
 * physical move or the attacker's SpA if the move is a special move. *)
val calc_attackers_attack : steammon -> move -> int

(* calc_defenders_defense defender att returns defender's Def if move is a 
 * physical move or the defender's SpD if the move is a special move. *)
val calc_defenders_defense : steammon -> move -> int

(* get_move_from_steammon mon move_name returns Some move if mon has a
 * move whose name matches move_name, otherwise None *)
val get_move_from_steammon : steammon -> string -> move option