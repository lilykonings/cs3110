(* Represents the type/element of a Steammon or move.*)
type steamtype = Fire
  | Water
  | Ice
  | Grass
  | Poison
  | Normal
  | Flying
  | Psychic
  | Ghost
  | Dark
  | Steel
  | Rock
  | Ground
  | Electric
  | Bug
  | Dragon
  | Fighting
  | Typeless

(* Represents the color of one of the two teams. *)
type color = Red | Blue

(* Represents one of the stats. HP is not included because HP stat modifiers
 * do not exist. *)
type stat = Atk | Def | SpA | SpD | Spe

(* Represents an item in the game. *)
type item = Ether
  | MaxPotion
  | Revive
  | FullHeal
  | XAttack
  | XDefense
  | XSpeed

(* Represents an inventory of items. Each element in the list represents
 * the number of the corresponding item remaining in the inventory,
 * according to the order above.
 *
 * For instance, [1;0;0;0;0;2;0cri] is an inventory with one Ether and two
 * X Defenses. *)
type inventory = int list 

(* Represents a status that a Steammon could be inflicted with. *)
type status = 
  | Paralyzed
  | Poisoned
  | Asleep
  | Burned
  | Frozen
  | Confused

(* The modifier values for each of a Steammon's stats *)
type modifier = {
  attack_mod: int;
  defense_mod: int;
  spl_attack_mod: int;
  spl_defense_mod: int;
  speed_mod: int;
}

(* Represents the general target of a move. *)
type target = User | Opponent

type effect = 
  (* InflictStatus s inflicts the status s on the target. *)
  | InflictStatus of status
  (* StatModifier(s,i) modifies the target's stat s by a level i. *)
  | StatModifier of stat * int
  (* RecoverPercent i recovers the target's HP by i% of its max HP. *)
  | RecoverPercent of int 
  (* Recoil i causes the target to take recoil damage, i% of the damage dealt. 
   * You may assume that the target will always be the user. *)
  | Recoil of int
  (* DamagePercent i lowers the target's HP by i% of its max HP. *)
  | DamagePercent of int (* % of max HP *)
  (* HealStatus lst removes all the statuses in lst from the target. *)
  | HealStatus of status list
  (* RestorePP m i increases the PP all moves by i, up to the maximum. *)
  | RestorePP of int

(* Represents a move. *)
type move = {
  name: string;
  element: steamtype;
  target: target;
  max_pp: int;
  pp_remaining: int;
  power : int;
  accuracy: int;
  (* All the effects in the list have the give target.
   * The int is the chance that the effects occur.
   * Either all occurs, or none of them do. *)
  effects: (effect list * target * int) option
}

(* Represents a Steammon! *)
type steammon = {
  species: string;
  curr_hp : int;
  max_hp : int;
  first_type: steamtype option;
  second_type: steamtype option;
  first_move: move;
  second_move: move;
  third_move: move;
  fourth_move: move;
  attack: int;
  spl_attack : int;
  defense: int;
  spl_defense: int;
  speed: int;
  status: status option;
  mods: modifier;
  cost: int
}

(* Represents a team. The active steammon is the head of the list of Steammon.
   The final int is the number of credits this team has during the draft
   phase. *)
type team_data = steammon list * inventory * int
(* Represents the status of the game, in (red_team, blue_team) order. *)
type game_status_data = team_data * team_data
(* Represents all the possible moves in the game, in no particular order. *)
type move_set = move list
(* Represents all the available Steammon in the game, in no particular order. *)
type steam_pool = steammon list

type game_result = Winner of color | Tie

(* Result of an effect. Uses actual HP values instead of percentages. Effects that
 * did not deal with percentages keep the same arguments when converted to an effect
 * result. Used for GUI updates. *)
type effect_result =
  | InflictedStatus of status
  | StatModified of stat * int
  | Recovered of int
  | Recoiled of int
  | Damaged of int
  | HealedStatus of status
  | RestoredPP of int
  (* DamagedByStatus(i,s) indicates that the Steammon took i damage because of
   * a status s. *)
  | DamagedByStatus of int * status

(* Represents the result of an attack. Failed should be used when an attack
 * fails due to some status effect (in particular, Asleep, Frozen and Confused. *)
type hit_result = Hit | Miss | Failed of status 

(* Represents the effectiveness of a move. The type match-up of the attacking
 * type vs. a defending type determines effectiveness. *)
type effectiveness = Regular | SuperEffective | NotVeryEffective | Ineffective

(* Represents the result of performing a move. Used for GUI updates. *)
type move_result = {
  name: string;
  element: steamtype;
  from: color; (* performer of move *)
  toward: color; (* recipient of move *)
  damage: int;
  hit: hit_result;
  effectiveness: effectiveness;
  effects: (effect_result * color) list
}

(* GUI updates. *)
type update = InitGraphics of string * string
            | UpdateSteammon of (string*int*int*color)
            | SetChosenSteammon of string 
            | Move of move_result           
            | Item of string * effect_result * color * string
            | AdditionalEffects of (effect_result * color) list
            | SetFirstAttacker of color
            | Message of string

(* Control updates. *)
type control = GameStart
             | GameRequest
             | Team of color
             | GameEnd

(* Requests from the game to the players. *)
type request = 
    (* Sent during intialization to request the name the player 
     * wants to use in the GUI *)
  | TeamNameRequest
    (* Sent at the beginning of the battle phase, or when the active 
     * Steammon faints. *)
  | StarterRequest of game_status_data 
    (* Sent during the draft phase to request that the player 
       draft a Steammon. *)
  | PickRequest of color * game_status_data * move_set * steam_pool 
    (* Sent during the inventory phase to request that the player
       purchase an inventory. *)
  | PickInventoryRequest of game_status_data 
    (* Sent during the battle phase to request that the player
       chose its next action. *)
  | ActionRequest of game_status_data

(* Actions sent by the players to the game. *)
type action = 
    (* Sent in response to TeamNameRequest to indicate the name a player
     * wants to use in the GUI. *)
  | SendTeamName of string
    (* Sent in response to StarterRequest to chose an active Steammon. *)
  | SelectStarter of string
    (* Sent in response to PickRequest. The player specifies the name of the 
       Steammon it wants to draft for its team. *)
  | PickSteammon of string
    (* Sent in response to PickInventoryRequest. A player specifies an 
       inventory that it wants to purchase. *)
  | PickInventory of inventory
    (* Sent as a possible response to ActionRequest to change the player's
       active Steammon. *)
  | SwitchSteammon of string
    (* Sent as a possible response to ActionRequest to use an item
       on a Steammon. *)
  | UseItem of item * string
    (* Sent as a possible response to ActionRequest to command the
       player's active Steammon to use a move. *)
  | UseMove of string

type command = Control of control 
             | Action of action 
             | DoNothing 
             | Request of request 
             | Error of string

(* See handle_step in game.mli. *)
type game_output = game_result option * game_status_data * command option * command option
