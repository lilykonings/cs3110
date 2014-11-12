open Async.Std
open Warmup

let sec = Core.Std.sec

(** [job name t] is a blocking function that does the following: {ol
      {li prints "starting" and its name}
      {li waits for t seconds}
      {li prints "finishing" and its name}
      {li returns its name.} *)
let job name time () =
  print_endline (name^" starting");
  after (sec (float_of_int time)) >>= fun () ->
  print_endline (name^" finishing ");
  return name

(* Run fork *)
let fork_example () =
  print_endline "running fork example";
  print_endline "expected output (after 15 seconds):";
  print_endline "  0:05 zar";
  print_endline "  0:10 zardoz";
  print_endline "  0:15 zarraz";
  print_endline "";
  print_endline "results should be in order.";
  print_endline "";
  print_endline "actual output:";

  let ivar = Ivar.create () in
  upon (after (sec 5.0)) (fun () -> print_endline "zar"; Ivar.fill ivar "zar");
  fork (Ivar.read ivar)
       (fun zar -> after (sec 5.0)  >>= fun () -> print_endline @@ zar ^ "doz"; return ())
       (fun zar -> after (sec 10.0) >>= fun () -> print_endline @@ zar ^ "raz"; return ());

  after (sec 20.0)

(* Run deferred_map *)
let deferred_map_example () =
  print_endline "";
  print_endline "running deferred_map example";
  print_endline "expected output (after 15 seconds):";
  print_endline "  0:00 J1 starting";
  print_endline "  0:00 J2 starting";
  print_endline "  0:00 J3 starting";
  print_endline "  0:05 J2 finishing";
  print_endline "  0:10 J1 finishing";
  print_endline "  0:15 J3 finishing";
  print_endline "  0:15 results:";
  print_endline "  0:15 J1";
  print_endline "  0:15 J2";
  print_endline "  0:15 J3";
  print_endline "";
  print_endline "results should be in order.";
  print_endline "";
  print_endline "actual output:";

  deferred_map [("J1", 10); ("J2", 5); ("J3", 15)]
               (fun (name,time) -> job name time ())
    >>= fun outputs ->
  print_endline "results:";
  List.iter print_endline outputs;
  return ()

(* Run the examples *)
let _ = fork_example ()
        >>= fun _ -> deferred_map_example ()

(** Start the async scheduler *)
let _ =
  Scheduler.go ()

