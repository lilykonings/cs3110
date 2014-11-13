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
    let workers = AQueue.create () in 

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
              ignore (AQueue.push workers a);
              ())
        )
      ) in

    let rec map input =
      if ((!active) = 0) then
        raise (InfrastructureFailure) (* "No active workers!" *)
      else
        (AQueue.pop workers) >>= (fun (s,r,w) ->
          ignore (Request.send w (Request.MapRequest (input)));
          Response.receive r >>= (fun resp ->
            ignore (AQueue.push workers (s,r,w));
            (match resp with
              | `Eof ->
                ignore (Socket.shutdown s `Both);
                ignore (active := (!active) - 1);
                map input
              | `Ok result -> (match result with
                | Response.JobFailed e -> raise (MapFailure e)
                | Response.ReduceResult a ->
                    ignore (Socket.shutdown s `Both);
                    ignore (active := (!active) - 1);
                    map input
                | Response.MapResult a -> return a
              )
            )
          )
        ) in

    let rec reduce (k, intrs) =
      if ((!active) = 0) then
        raise (InfrastructureFailure) (* No active workers! *)
      else 
        (AQueue.pop workers) >>= (fun (s, r, w) ->
          ignore (Request.send w (Request.ReduceRequest (k, intrs)));
          Response.receive r >>= (fun resp ->
            ignore (AQueue.push workers (s, r, w));
            (match resp with
              | `Eof ->
                ignore (Socket.shutdown s `Both);
                ignore (active := (!active) - 1);
                reduce (k, intrs)
              | `Ok result -> (match result with
                | Response.JobFailed e -> raise (ReduceFailure e)
                | Response.MapResult _ ->
                    ignore (Socket.shutdown s `Both);
                    ignore (active := (!active) - 1);
                    reduce (k, intrs)
                | Response.ReduceResult a -> return (k, a)
              )
            )
          )
        ) in
    
    connect ()
    >>= (fun _ -> Deferred.List.map inputs map)
    >>| List.flatten
    >>| Combine.combine
    >>= fun mapped -> Deferred.List.map mapped reduce

end