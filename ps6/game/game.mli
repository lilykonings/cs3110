open Definitions
(*
 * This type serves as your book-keeping variable that will keep track
 * of the game. It will represent the current state of the game.
 *) 
type game
(*
 * Convert your game to the game_status_data defined in definitions.ml
 *)
val game_datafication: game -> game_status_data

(*
 * Convert to your implementation of type game from game_status_data
 *)
val game_from_data: game_status_data -> game

(* init_game generates a blank copy of the game, returning (gs * r1 * r2 
 * al * sl).
 * gs is a game with inventories initialized, and neither player with any
 *   steammon.
 * r1 is the first request sent out to the red team
 * r2 is the first request sent out to the blue team
 * al is the list of all attacks, as read from attack.txt
 * sl is the list of all steammon as read from steammon.txt
 *)
val init_game: unit -> game * request * request * move list * steammon list

(* handle_step gs ra ba is used to generate the next state of the game.
 * gs is the current game state entering this turn.
 * ra and ba are the Action commands sent by the red team and the blue
 *   team, respectively. If ra and ba are not Action(action) commands,
 *   the game will ignore the command given.
 * handle_step outputs game_output (gr, gs, rr, br) where:
 * gr is the result of the game. Output None if no winner was determined.
 * gs is the state of the game after handle_step ended.
 * rr is an option containing the request for the red team
 * br is an option containing the request for the blue team
 * None indicates that the team should respond with DoNothing
 *)
val handle_step: game -> command -> command -> game_output
