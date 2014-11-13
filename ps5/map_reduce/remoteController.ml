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
        (try_with (fun () -> Tcp.connect addr)
        >>= function
          | Core.Std.Error e -> return ()
          | Core.Std.Ok (s,r,w) ->
            active := (!active) + 1; return ()
            >>= (fun _ ->
              Writer.write_line w Job.name;
              return ())
            >>= (fun _ ->
              AQueue.push workers (s,r,w);
              return ())
        )
      ) in

    let rec map input =
      if ((!active) = 0) then
        raise (InfrastructureFailure) (* No connection with any workers *)
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
                | Response.ReduceResult _ -> raise (MapFailure "Wrong response type! Expecting MapResult, got ReduceResult")
                | Response.MapResult a -> return a
              )
            )
          )
        ) in

    let rec reduce (k, intrs) =
      if ((!active) = 0) then
        raise (InfrastructureFailure) (* No connection with any workers *)
      else 
        (AQueue.pop workers) >>= (fun (s,r,w) ->
          ignore (Request.send w (Request.ReduceRequest (k, intrs)));
          Response.receive r >>= (fun resp ->
            ignore (AQueue.push workers (s,r,w));
            (match resp with
              | `Eof ->
                  ignore (Socket.shutdown s `Both);
                  ignore (active := (!active) - 1);
                  reduce (k, intrs)
              | `Ok result -> (match result with
                | Response.JobFailed e -> raise (ReduceFailure e)
                | Response.MapResult _ -> raise (ReduceFailure "Wrong response type! Expecting ReduceResult, got MapResult.")
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
