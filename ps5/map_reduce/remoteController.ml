open Async.Std

let addresses = ref []

let init addrs : () =
  addresses :=
    List.map (fun (s,i) -> Tcp.to_host_and_port s i) addrs;

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

  let map_reduce inputs =
    let active = ref 0 in
    let queue = AQueue.create () in 

    let connect () =
    Deferred.List.map (!addresses) (fun addr ->
      (try_with (fun () -> Tcp.connect addr))
      >>= function
        | Core.Std.Error _ -> return ()
        | Core.Std.Ok (_,_,w) -> Deferred.all (Writer.write_line w Job.name)
      >>= (fun a -> active := (!active) + 1; AQueue.push cons a)
    )

end

