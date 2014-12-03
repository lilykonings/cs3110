(* Note: All probability constants represent a percentage. *)

(********************************************************************************
 * General and initialization constants											*
 ********************************************************************************)

let cNUM_TEAMS = 2
(* How long the server waits for a player response (excluding PickRequests), 
 * in seconds *)
let cUPDATE_TIME = 0.5
(* How long the server waits for players to make picks, in seconds. *)
let cPICK_UPDATE_TIME = 1.0
(* How long the server waits between sending updates to the GUI, in seconds *)
let cGUI_UPDATE_TIME = 0.2

let cDEFAULT_PORT_NUMBER = 10500
let cDEFAULT_GUI_PORT_NUMBER = 10501
let cDEFAULT_BOT_PORT_NUMBER = 10502

(* The number of Steammon on each player's team. Files containing Steammon
 * data should always have at least 2 * cNUM_PICKS Steammon. *)
let cNUM_PICKS = 6
(* Number of credits that each player is given to "purchase" their picks with.
 * When a Steammon is picked, the cost of the Steammon should be deducted from
 * the player's credits. *)
let cSTEAMMON_CREDITS = 1500

(********************************************************************************
 * Item constants																*
 ********************************************************************************)

let cINITIAL_CASH = 3000
let cCOST_ETHER = 200
let cCOST_MAXPOTION = 500
let cCOST_FULLHEAL = 350
let cCOST_REVIVE = 950
let cCOST_XATTACK = 400
let cCOST_XDEFEND = 300
let cCOST_XSPEED = 300

(* Default inventory *)
let cNUM_ETHER = 1
let cNUM_MAX_POTION = 1
let cNUM_REVIVE = 1
let cNUM_FULL_HEAL = 1
let cNUM_XATTACK = 1
let cNUM_XDEFENSE = 1
let cNUM_XSPEED = 1

(********************************************************************************
 * Move constants																*
 ********************************************************************************)

(* Multiplier for Same-Type Attack Bonus, which is applied when a Steammon
 * uses a move that has the same Steamtype as one of its own types. *)
let cSTAB_BONUS = 1.5
(* Lower bound on the random multiplier for damage. Allows for some variation
 * in damage, even with all factors held constant. *)
let cMIN_DAMAGE_RANGE = 85

(********************************************************************************
 * Type matchup constants														*
 ********************************************************************************)

(* Multipliers for type matchups *)
let cSUPER_EFFECTIVE = 2.
let cNOT_VERY_EFFECTIVE = 0.5
let cINEFFECTIVE = 0.
let cREGULAR = 1.

(********************************************************************************
 * Status constants																*
 ********************************************************************************)

(* See writeup for details about how to use these constants. *)
let cPOISON_DAMAGE = 0.06
let cBADLY_POISONED_INIT_DAMAGE = 0.03
let cPARALYSIS_CHANCE = 20
let cPARALYSIS_SLOW = 4
let cBURN_WEAKNESS = 0.5
let cBURN_DAMAGE = 0.12
let cWAKE_UP_CHANCE = 50
let cDEFROST_CHANCE = 20
let cSELF_ATTACK_CHANCE = 50
let cSNAP_OUT_OF_CONFUSION = 40
let cSELF_ATTACK_POWER = 45