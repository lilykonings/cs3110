open Async.Std

let addresses = ref []

let init addrs : unit =
  addresses := List.map (fun (s,i) ->
    Tcp.to_host_and_port s i) addrs;

exception InfrastructureFailure
exception MapFailure of string
exception ReduceFailure of string

(* Remember that map_reduce should be parallelized. Note that [Deferred.List]
 * functions are run sequentially by default. To achieve parallelism, you may
 * find the data structures and functions you implemented in the warmup
 * useful. Or, you can pass [~how:`Parallel] as an argument to the
 * [Deferred.List] functions.*)
module Make (Job : MapReduce.Job) = struct
  module Request = Protocol.WorkerRequest(Job)
  module Response = Protocol.WorkerResponse(Job)
  module Combine = Combiner.Make(Job)

  let map_reduce inputs =
    let active = ref 0 in
    let queue = AQueue.create () in 

    let connect () =
      Deferred.List.map (!addresses) (fun addr ->
        (try_with (fun () -> Tcp.connect addr))
        >>= (function
          | Core.Std.Error _ -> return ()
          | Core.Std.Ok (s,r,w) ->
            (try_with (fun () -> return (Writer.write_line w Job.name)))
              >>| (function
                | Core.Std.Error _ -> failwith "Writer's block"
                | Core.Std.Ok _ -> (s,r,w))
            >>| (fun a ->
              ignore (active := (!active) + 1);
              ignore (AQueue.push queue a);
              ()))
      ) in

    let rec reduce (k, intrs) =
      if ((!active) = 0) then
        raise (InfrastructureFailure) (* No active workers! *)
      else 
        (AQueue.pop queue) >>= (fun (s, r, w) ->
          ignore (Request.send w (Request.ReduceRequest (k, intrs)));
          Response.receive r >>= (fun resp ->
            ignore (AQueue.push queue (s, r, w));
            (match resp with
              | `Eof ->
                ignore (Socket.shutdown s `Both);
                ignore (active := (!active) - 1);
                reduce (k, intrs)
              | `Ok result -> (match result with
                | Response.JobFailed e -> raise (ReduceFailure e)
                | Response.MapResult _ ->
                  (* In this case, should you just drop the worker and
                  attempt to perform a reduce with another one? *) 
                  raise (ReduceFailure "ReduceResult expected, but receieved
                    MapResult instead.")
                | Response.ReduceResult a -> 
                  Socket.shutdown s `Both;
                  active := (!active) - 1;
                  return (k, a) (* Why does this need to be deferred.t? *)
              )
            )
          )
        ) in

    let rec map input =
      if ((!active) = 0) then
        raise (InfrastructureFailure) (* "No active workers!" *)
      else
        (AQueue.pop queue) >>= (fun (s,r,w) ->
          ignore (Request.send w (Request.MapRequest (input)));
          Response.receive r >>= (fun resp ->
            ignore (AQueue.push queue (s,r,w));
            (match resp with
              | `Eof ->
                ignore (Socket.shutdown s `Both);
                ignore (active := (!active) - 1);
                (* workers are considered active after being connected to 
                and so it makes sense to decrement here as it is now not 
                connected...now how to reconnect to it? (are we even
                supposed to try to continue using that worker?) *)
                map input
              | `Ok result -> (match result with
                | Response.JobFailed e -> raise (MapFailure e)
                | Response.ReduceResult a -> (* Should this be here? 
                  in what scenario would you get a ReduceResult back after
                  sending a MapRequest? Should it just raise MapFailure*)
                  Socket.shutdown s `Both;
                  active := (!active) - 1;
                  raise (MapFailure "MapResult expected, but receieved
                    ReduceResult instead.")
                  (* assign_work input *)  (* what is this? *)
                | Response.MapResult a -> (* Need to do reduce stage *)
                  return a (* Why does this need to be deferred.t? *)
              )
            )
          )
        ) in
    
    let helpMap a = 
      (match (Deferred.peek (map a)) with
      | Some x -> x 
      | None -> raise (InfrastructureFailure)) in

    let helpRed a = 
      (match (Deferred.peek (reduce a)) with
      | Some x -> x 
      | None -> raise (InfrastructureFailure)) in
    
    ignore (connect ());
    return (List.map helpRed (Combine.combine (List.flatten (List.map helpMap inputs))))

end
