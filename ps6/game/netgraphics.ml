open Definitions
open Util

let clients = ref []
let updates = ref []
let clients_lock = Mutex.create()
let updates_lock = Mutex.create()

let string_of_hit_result = function
  | Hit -> "Hit"
  | Miss -> "Miss"
  | Failed Frozen -> "FailedFrozen"
  | Failed Asleep -> "FailedAsleep"
  | Failed Paralyzed -> "FailedParalyzed"
  | Failed _ -> failwith "this status can't block a move"
  
let string_of_effectiveness = function
  | Regular -> "Regular"
  | SuperEffective -> "SuperEffective"
  | NotVeryEffective -> "NotVeryEffective"
  | Ineffective -> "Ineffective"

let parse_updates updates =
  let concat l = String.concat "#" l in
  let concat_2 l = String.concat "," l in
  let string_of_team t = if t = Red then "Red" else "Blue" in
  let filter s =
    let new_s = ref [] in
      String.iter (fun c -> 
        if c <> '#' && c <> '$' then new_s := String.make 1 c::(!new_s) else ()) s;
      String.concat "" (List.rev !new_s) in
  let parse u =
    match u with
      InitGraphics(r_name, b_name) -> concat ["InitGraphics"; 
                                              r_name; 
                                              b_name]
    | UpdateSteammon (s,hp,max,t) -> concat ["UpdateSteammon";
                                             s;
                                             string_of_int hp;
                                             string_of_int max;
                                             string_of_team t]
    | SetChosenSteammon s -> concat ["SetChosenSteammon"; s]
    | Move res -> let (effs,colors) = List.split res.effects in
                    concat ["Move"; 
                            res.name;
                            string_of_type res.element;
                            string_of_color res.from;
                            string_of_color res.toward;
                            string_of_int res.damage;
                            string_of_hit_result res.hit;
                            string_of_effectiveness res.effectiveness;
                            concat_2 (List.map string_of_effect_result effs);
                            concat_2 (List.map string_of_color colors)]
    | Item (item_name, eff,c,mon_name) -> concat ["Item";
                                          string_of_effect_result eff;
                                          string_of_color c;
                                          mon_name;
                                          item_name]
    | AdditionalEffects lst ->  
      let (effs,colors) = List.split lst in
      concat ["AdditionalEffects";
              concat_2 (List.map string_of_effect_result effs);
              concat_2 (List.map string_of_color colors)]
    | SetFirstAttacker t -> concat ["SetFirstAttacker"; string_of_team t]
    | Message s -> concat ["Message"; filter s]
  in
  let s = String.concat "" (List.map (fun u -> "$" ^ (parse u) ^ "$") updates) in
    s

let add_clients server =
  while true do
    let (c, a) = Unix.accept server in
      Mutex.lock clients_lock;
      print_endline "A client connected to gui server";
      clients := (Connection.server a c)::!clients;
      Mutex.unlock clients_lock;
  done

let init_server port =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt server Unix.SO_REUSEADDR true;
    Unix.setsockopt server Unix.SO_KEEPALIVE false;
    Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen server 100;
    server

let init_single_connection port =
  let server = init_server port in
  let (c, a) = Unix.accept server in
    Mutex.lock clients_lock;
    print_endline "A client connected to gui server";
    clients := (Connection.server a c)::!clients;
    Mutex.unlock clients_lock;
    ignore(Thread.create add_clients server)

let init port = ignore(Thread.create add_clients (init_server port))

let add_update u =
  Mutex.lock updates_lock;
  updates := u::(!updates);
  Mutex.unlock updates_lock

let send u =
  Mutex.lock clients_lock;
  let parsed_updates = parse_updates u in
    (try
       clients := List.fold_left (fun new_clients c ->
        if Connection.output_string c parsed_updates then
          let _ = () in c::new_clients
        else
          (Connection.close c; new_clients)) [] !clients;
       ()
     with _ -> ());
    Mutex.unlock clients_lock

let send_update u = send [u]

let send_updates () =
  Mutex.lock updates_lock;
  let u = List.rev !updates in
    updates := [];
    Mutex.unlock updates_lock;
    send u
