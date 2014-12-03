open Definitions
open Constants

(********************************************************************************
 * Hashtable with key = string                                                  *
 ********************************************************************************)

module Sstring = struct
  type t = string
  let hash s1 = Hashtbl.hash s1
  let equal s1 s2 = String.compare s1 s2 = 0
end

module Table = Hashtbl.Make(Sstring)

let hash_to_list ht = Table.fold(fun k v acc -> v::acc) ht []

(********************************************************************************
 * Calculation functions (Important!)                                           *
 ********************************************************************************)

let multiplier_of_modifier stage =
  if stage >= 0 then
    (float_of_int (stage + 2)) /. 2.
  else
    2. /. (float_of_int ((abs stage) + 2))

let calculate_damage atk def pwr multiplier =
  int_of_float (((float_of_int (2 * 100 + 10)) /. 250. *. (float_of_int atk 
    /. float_of_int def) *. ((float_of_int pwr) +. 2.)) *. multiplier)

let weakness type1 type2 : effectiveness =
  match type1 with
  | Normal->
     (match type2 with
      | Rock | Steel -> NotVeryEffective
      | Ghost -> Ineffective
      | _ -> Regular)
  | Fire ->
     (match type2 with
      | Fire | Water | Rock | Dragon -> NotVeryEffective
      | Grass | Ice | Bug | Steel -> SuperEffective
      | _ -> Regular)
  | Water ->
     (match type2 with
      | Fire | Ground | Rock -> SuperEffective
      | Water | Grass | Dragon -> NotVeryEffective
      | _ -> Regular)
  | Grass ->
     (match type2 with
      | Fire | Grass | Poison | Flying 
      | Bug | Dragon |Steel -> NotVeryEffective
      | Water | Ground | Rock -> SuperEffective
      | _ -> Regular)
  | Electric ->
     (match type2 with
      | Water | Flying -> SuperEffective
      | Grass | Electric | Dragon -> NotVeryEffective
      | Ground -> Ineffective
      | _ -> Regular)
  | Ice ->
     (match type2 with
      | Fire | Water | Ice | Steel -> NotVeryEffective
      | Grass | Ground | Flying | Dragon -> SuperEffective
      | _ ->Regular)
  | Fighting ->
     (match type2 with
      | Normal | Ice | Rock | Dark | Steel -> SuperEffective
      | Poison | Flying | Psychic | Bug-> NotVeryEffective
      | Ghost -> Ineffective
      | _ -> Regular)
  | Poison ->
     (match type2 with
      | Grass-> SuperEffective
      | Poison | Ground | Rock | Ghost -> NotVeryEffective
      | Steel -> Ineffective
      | _ -> Regular)
  | Ground ->
     (match type2 with
      | Fire | Electric | Poison | Rock | Steel-> SuperEffective
      | Grass | Bug -> NotVeryEffective
      | Flying -> Ineffective
      | _ -> Regular)
  | Flying ->
     (match type2 with
      | Grass | Fighting | Bug-> SuperEffective
      | Electric | Rock | Steel -> NotVeryEffective
      | _ -> Regular)
  | Psychic ->
     (match type2 with
      | Fighting | Poison -> SuperEffective
      | Psychic | Steel -> NotVeryEffective
      | Dark -> Ineffective
      | _ -> Regular)
  | Bug ->
     (match type2 with
      | Fire | Fighting | Poison
      | Flying | Ghost | Steel -> NotVeryEffective
      | Grass | Psychic | Dark -> SuperEffective
      | _ ->Regular)
  | Rock ->
     (match type2 with
      | Fire | Ice | Flying | Bug -> SuperEffective
      | Fighting | Ground | Steel -> NotVeryEffective
      | _ -> Regular)
  | Ghost ->
     (match type2 with
      | Psychic | Ghost -> SuperEffective
      | Dark | Steel -> NotVeryEffective
      | Normal -> Ineffective
      | _ -> Regular)
  | Dragon ->
     (match type2 with
      | Dragon -> SuperEffective
      | Steel -> NotVeryEffective
      | _ -> Regular)
  | Dark ->
     (match type2 with
      | Poison | Dark | Steel -> NotVeryEffective
      | Psychic | Ghost -> SuperEffective
      | _ -> Regular)
  | Steel ->
     (match type2 with
      | Fire | Water | Electric | Steel -> NotVeryEffective
      | Ice | Rock -> SuperEffective
      | _ -> Regular)
  | Typeless -> Regular

let multiplier_of_effectiveness = function
  | SuperEffective -> cSUPER_EFFECTIVE
  | NotVeryEffective -> cNOT_VERY_EFFECTIVE
  | Ineffective -> cINEFFECTIVE
  | Regular -> cREGULAR

let calculate_type_matchup move_type (mon_type1, mon_type2) =
  let unwrap_type = function
    | Some typ -> typ
    | None -> Typeless in
  let combine_effectiveness e1 e2 = 
    match (e1, e2) with
    | (Ineffective, _) 
    | (_, Ineffective) -> Ineffective
    | (Regular, Regular) 
    | (NotVeryEffective, SuperEffective) 
    | (SuperEffective, NotVeryEffective) -> Regular
    | (NotVeryEffective, Regular)
    | (Regular, NotVeryEffective) 
    | (NotVeryEffective, NotVeryEffective) -> NotVeryEffective
    | (SuperEffective, Regular)
    | (Regular, SuperEffective) 
    | (SuperEffective, SuperEffective) -> SuperEffective in

  let e1 = weakness move_type (unwrap_type mon_type1) in
  let e2 = weakness move_type (unwrap_type mon_type2) in
  let overall_effectiveness = combine_effectiveness e1 e2 in
  let multiplier = 
    (multiplier_of_effectiveness e1) *. (multiplier_of_effectiveness e2) in
  (overall_effectiveness, multiplier)

(********************************************************************************
 * String conversion and file parsing functions                                 *
 ********************************************************************************)

let type_of_string str = match str with
  | "Fire" -> Fire
  | "Water" -> Water
  | "Ice" -> Ice
  | "Grass" -> Grass
  | "Poison" -> Poison
  | "Normal" -> Normal
  | "Flying" -> Flying
  | "Psychic" -> Psychic
  | "Ghost" -> Ghost
  | "Dark" -> Dark
  | "Steel" -> Steel
  | "Rock" -> Rock
  | "Ground" -> Ground
  | "Electric" -> Electric
  | "Bug" -> Bug
  | "Dragon" -> Dragon
  | "Fighting" -> Fighting
  | "Typeless" -> Typeless
  | _ -> failwith "invalid steamtype string"

let stat_of_string str = match str with
  | "Atk" -> Atk 
  | "Def" -> Def 
  | "SpA" -> SpA 
  | "SpD" -> SpD 
  | "Spe" -> Spe
  | _ -> failwith "invalid stat string"

let string_of_color c =
  match c with
  | Red -> "Red"
  | Blue -> "Blue"
  
let item_of_string = function
  | "Ether" -> Ether
  | "MaxPotion" -> MaxPotion
  | "Revive" -> Revive
  | "FullHeal" -> FullHeal
  | "XAttack" -> XAttack
  | "XDefense" -> XDefense
  | "XSpeed" -> XSpeed
  | _ -> failwith "invalid item string"

let string_of_type typ = match typ with
  | Fire -> "Fire"
  | Water -> "Water"
  | Ice -> "Ice"
  | Grass -> "Grass"
  | Poison -> "Poison"
  | Normal -> "Normal"
  | Flying -> "Flying"
  | Psychic -> "Psychic"
  | Ghost -> "Ghost"
  | Dark -> "Dark"
  | Steel -> "Steel"
  | Rock -> "Rock"
  | Ground -> "Ground"
  | Electric -> "Electric"
  | Bug -> "Bug"
  | Dragon -> "Dragon"
  | Fighting -> "Fighting"
  | Typeless -> "Typeless"

let string_of_stat stat = match stat with
  | Atk -> "Atk" 
  | Def -> "Def" 
  | SpA -> "SpA" 
  | SpD -> "SpD" 
  | Spe -> "Spe"

let status_of_string str = match str with
  | "Paralyzed" -> Paralyzed
  | "Poisoned" -> Poisoned
  | "Asleep" -> Asleep
  | "Burned" -> Burned
  | "Frozen" -> Frozen
  | "Confused" -> Confused
  | _ -> failwith "invalid status string"

let string_of_status status = match status with
  | Paralyzed -> "Paralyzed"
  | Poisoned -> "Poisoned"
  | Asleep -> "Asleep"
  | Burned -> "Burned"
  | Frozen -> "Frozen"
  | Confused -> "Confused"

let string_of_effect eff = match eff with
  | InflictStatus(status) -> "InflictStatus "^(string_of_status status)
  | StatModifier(stat,i) -> "StatModifier "^(string_of_stat stat)^" "
    ^(string_of_int i)
  | RecoverPercent(i) -> "RecoverPercent "^(string_of_int i)
  | Recoil(i) -> "Recoil "^(string_of_int i)
  | DamagePercent(i) -> "DamagePercent "^(string_of_int i)
  | HealStatus(lst) -> List.fold_left (fun acc status -> 
      acc^" "^(string_of_status status)) "HealStatus" lst
  | RestorePP(i) -> "RestorePP "^(string_of_int i)

let string_of_effect_result res = match res with
  | InflictedStatus(status) -> "InflictedStatus "^(string_of_status status)
  | StatModified(stat,i) -> "StatModified "^(string_of_stat stat)^" "
    ^(string_of_int i)
  | Recovered(i) -> "Recovered "^(string_of_int i)
  | Recoiled(i) -> "Recoiled "^(string_of_int i)
  | Damaged(i) -> "Damaged "^(string_of_int i)
  | HealedStatus(status) -> "HealedStatus "^(string_of_status status)
  | DamagedByStatus(i,status) -> "DamagedByStatus "^(string_of_int i)^" "
    ^(string_of_status status)
  | RestoredPP(i) -> "RestoredPP "^(string_of_int i)

let string_of_target target = match target with
  | User -> "User"
  | Opponent -> "Opponent"

let string_of_item it =
  match it with
  | Ether -> "Ether"
  | MaxPotion -> "MaxPotion"
  | Revive -> "Revive"
  | FullHeal -> "FullHeal"
  | XAttack -> "XAttack"
  | XDefense -> "XDefense"
  | XSpeed -> "XSpeed"

let wordify delim str =
  let rec remove_leading_delims delim str =
  if String.get str 0 = delim then
    remove_leading_delims delim (String.sub str 1 ((String.length str) - 1))
  else
    str in
  let rec helper str lst =
    if(String.contains str delim) then
      let last_space = String.rindex str delim in
      let word = String.sub str last_space ((String.length str) - last_space) in
      helper (String.sub str 0 last_space) (word::lst)
    else
      str::lst in
  List.map (remove_leading_delims delim) (helper str [])

let read_lines filename =
  Printf.printf "READING %s\n" filename;
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      if line.[0] <> '#' then lines := line :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !lines

(********************************************************************************
 * Helper functions (for your convenience)                                      *
 ********************************************************************************)

let invert_color c = 
  match c with
  | Red -> Blue
  | Blue -> Red

let is_special = function
  | Fire | Water | Psychic | Grass | Electric | Dragon | Ice | Dark -> true
  | _ -> false

let calc_attackers_attack (attacker:steammon) (att : move) : int =
  if is_special att.element then attacker.spl_attack
  else attacker.attack

let calc_defenders_defense (defender:steammon) (att : move) : int =
  if is_special att.element then defender.spl_defense
  else defender.defense

let get_move_from_steammon mon move_name = 
  if mon.first_move.name = move_name then Some mon.first_move else
  if mon.second_move.name = move_name then Some mon.second_move else 
  if mon.third_move.name = move_name then Some mon.third_move else
  if mon.fourth_move.name = move_name then Some mon.fourth_move else
  None