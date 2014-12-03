open Constants
open Definitions
open Util

(* Hashtable that maps names of moves to the actual moves themselves. Call
 * init_pool to initialize. *)
val move_table : move Table.t

(* Hashtable that maps names of Steammon to the actual Steammon themselves.
 * Call init_pool to initialize. *)
val mon_table : steammon Table.t

(* init_pool moves_file mons_file initializes the pools of moves and 
 * Steammon by populating them with the moves in moves_file and the Steammon
 * in mons_file. *)
val init_pool : string -> string -> unit