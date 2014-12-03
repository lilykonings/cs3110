open Definitions
open Constants
open Util

let host = try (Unix.gethostbyname ((Sys.argv).(1))).Unix.h_addr_list.(0)
           with _ -> failwith ("Must provide hostname of game server as " ^
                               "first command-line argument for team to " ^
                               "connect to.")
let port = try int_of_string ((Sys.argv).(2))
           with _ -> cDEFAULT_BOT_PORT_NUMBER
let addr = Unix.ADDR_INET (host, port)
let retries = 10
let conn = ref None
let mutex = Mutex.create ()
let _ = Random.self_init ()

let running = ref true
let lastReceivedMsg : command option ref = ref None
let msgLock = Mutex.create ()

(* -------------------- Communication Stuff ------------------ *)
(* Doesn't return until a connection is established. *)
let recreate_conn () =
  let conn' = ref None in
  while !conn' = None do
    conn' := Connection.init addr retries
  done ; conn := !conn' ;
  match !conn with
     Some conn' -> conn'
   | None -> failwith "could not create connection in recreateConn"


let get_conn () =
  match !conn with
     Some conn -> conn
   | None -> recreate_conn ()

let set_conn c = conn := c

let (toSend : command option ref) = ref None
let skip = ref false

let rec listen_loop () =
  let wait_for_input () =
    let request = Connection.input (get_conn ()) in
    (match request with
      | Some r ->
          Mutex.lock msgLock;
          lastReceivedMsg := Some r;
          Mutex.unlock msgLock;
          listen_loop ()
      | None ->
          running := false;
          print_endline "Connection to game server closed.");
  in
  match (Mutex.lock msgLock; (!skip, !toSend)) with
    | (false, None) ->
        Mutex.unlock msgLock;
        Thread.yield ();
        listen_loop ()
    | (true, _) ->
        Mutex.unlock msgLock;
        skip := false;
        wait_for_input ()
    | (false, (Some msg)) ->
        toSend := None;
        Mutex.unlock msgLock;
        if not (Connection.output (get_conn ()) msg) then
          (running := false; failwith "could not send msg")
        else
          wait_for_input ()


let wait_for_game_start () =
  skip := true;
  while (!lastReceivedMsg = None && !running) do Thread.yield () done;
  match !lastReceivedMsg with
  | Some (Control(GameStart)) -> lastReceivedMsg := None
  | _ -> failwith "didn't receive GameStart"


let is_none opt = match opt with None -> true | _ -> false

let rec bot_loop (c: color) (bf : color -> request -> action) =
  while (Mutex.lock msgLock; !running && is_none (!lastReceivedMsg)) do
    (Mutex.unlock msgLock;
    Thread.yield ())
  done;
  let over = ref false in
  let o = !lastReceivedMsg in
  lastReceivedMsg := None;
  Mutex.unlock msgLock;
  (match o with
   | Some (Request v) ->
       Mutex.lock msgLock;
       toSend := Some (Action (bf c v));
       Mutex.unlock msgLock
   | Some (Control GameEnd) ->
       print_endline "Game over.";
       over := true
   | Some (DoNothing) -> print_endline "Received DoNothing, confused"
   | None -> over := true; ()
   | _ -> failwith "unknown msg");

  if not !over then bot_loop c bf

let run_bot (bf: color -> request -> action) =
  conn := Connection.init addr 10;
  match (!conn) with
    Some conn ->
      toSend := Some (Control(GameRequest));
      ignore (Thread.create (listen_loop) ());
      while (!lastReceivedMsg = None) do Thread.yield () done;
      (match !lastReceivedMsg with
        Some(Control(Team(c))) ->
          print_endline ("Bot has color "^(string_of_color c)) ;
          lastReceivedMsg := None;
          wait_for_game_start ();
          skip := true;
          bot_loop c bf
        | _ -> failwith "Server didn't respond to game request with team")
  | None -> failwith "Can't connect to server"



(* --------------------- Helper Functions -------------------- *)
let val_of x = match x with
  | Some(y) -> y
  | None -> failwith "Tried to val_of None"

