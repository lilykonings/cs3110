open Constants
open Definitions
open Util
	
let move_table = Table.create 100
let mon_table = Table.create 45

let effect_list_of_string str : effect list * target * int =
  let lst = wordify ' ' str in
  let rec helper acc lst = match lst with
    | [] -> failwith "invalid effects string (missing target?)"
    | hd::hd'::tl -> begin match hd with
      | "RecoverPercent" -> 
        helper ((RecoverPercent (int_of_string hd'))::acc) tl
      | "Recoil" -> helper ((Recoil (int_of_string hd'))::acc) tl
      | "DamagePercent" -> helper ((DamagePercent(int_of_string hd'))::acc) tl
      | "InflictStatus" -> 
        helper ((InflictStatus(status_of_string hd'))::acc) tl
      | "StatModifier" -> begin match tl with
        | hd''::tl' -> helper ((StatModifier((stat_of_string hd'),
          (int_of_string hd'')))::acc) tl'
        | [] -> failwith "invalid effect string (StatModifier)"
        end
      | "HealStatus" -> 
        let rec get_following_statuses acc lst = match lst with 
          | [] -> failwith "invalid effect string (HealStatus)"
          | hd::tl -> try get_following_statuses ((status_of_string hd)::acc) tl
            with _ -> (acc, lst)
        in
        let (statuses, lst') = get_following_statuses [] (hd'::tl) in
        helper ((HealStatus (List.rev statuses))::acc) lst'
      | "User" -> (List.rev acc, User, (int_of_string hd'))
      | "Opponent" -> (List.rev acc, Opponent, (int_of_string hd'))
      | _ -> failwith ("invalid effects string " ^ hd)
      end
    | _ -> failwith "invalid effects string"
  in helper [] lst

let target_of_string = function
  | "User" -> User
  | "Opponent" -> Opponent
  | _ -> failwith "invalid target string"

let line_to_move word_lst = match word_lst with
  | nm::typ::tar::pow::acc::pp::tl -> 
    let move = {name = nm;
              element = type_of_string typ;
              target = target_of_string tar;
              max_pp = int_of_string pp;
              pp_remaining = int_of_string pp;
              power = int_of_string pow;
              accuracy = int_of_string acc;
              effects = 
                match tl with
                | [] -> None
                | effs::[] -> Some (effect_list_of_string effs)
                | _ -> failwith "invalid input file"} in
    Table.add move_table nm move
  | _ -> failwith "Invalid input file!"

let to_type str =
  try (Some(type_of_string str))
  with e -> None

let string_of_type_opt = function
  | Some typ -> string_of_type typ
  | None -> ""

let string_of_mon mon =
  String.concat " " [mon.species; 
                      string_of_type_opt mon.first_type;
                      string_of_type_opt mon.second_type;
                      string_of_int mon.max_hp;
                      string_of_int mon.attack;
                      string_of_int mon.spl_attack;
                      string_of_int mon.defense;
                      string_of_int mon.spl_defense;
                      string_of_int mon.speed;
                      mon.first_move.name;
                      mon.second_move.name;
                      mon.third_move.name;
                      mon.fourth_move.name;
		      string_of_int mon.cost]

let create_modifiers () =
  {attack_mod = 0; speed_mod = 0; defense_mod = 0;
     spl_attack_mod = 0; spl_defense_mod = 0}

let line_to_mon word_lst =
  match word_lst with
  | [sp; ft; st; hp; atk; def; spl_atk; spl_def; spd; fa; sa; ta; foa; c] ->
      let mon = {species = sp;
                 curr_hp = int_of_string hp;
                 max_hp = int_of_string hp;
                 first_type = to_type ft;
                 second_type = to_type st;
                 first_move =  (Table.find move_table fa);
                 second_move =  (Table.find move_table sa);
                 third_move =  (Table.find move_table ta);
                 fourth_move =  (Table.find move_table foa);
                 attack = int_of_string atk;
                 spl_attack = int_of_string spl_atk;
                 defense = int_of_string def;
                 spl_defense = int_of_string spl_def;
                 speed = int_of_string spd;
                 status = None;
                 mods = create_modifiers ();
                 cost = int_of_string c} in
        Table.add mon_table sp mon
  | _ -> ()

let init_pool moves_file mons_file =
  let move_lines = read_lines moves_file in
  let moves = List.map (wordify ',') move_lines in
  List.iter line_to_move moves;
  let mon_lines = read_lines mons_file in
  let mons = List.map (wordify ',') mon_lines in
  List.iter line_to_mon mons;
  print_endline "finished!"
	
