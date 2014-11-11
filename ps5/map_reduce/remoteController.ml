open Async.Std

let addresses = ref []

let init addrs =
  ignore (
  	List.iter (fun x -> addresses := (x::(!addresses))) addrs
  );

exception InfrastructureFailure
exception MapFailure of string
exception ReduceFailure of string

module Make (Job : MapReduce.Job) = struct
  (* Remember that map_reduce should be parallelized. Note that [Deferred.List]
     functions are run sequentially by default. To achieve parallelism, you may
     find the data structures and functions you implemented in the warmup
     useful. Or, you can pass [~how:`Parallel] as an argument to the
     [Deferred.List] functions.*)
  module C = Combiner.Make(Job)
  module WorkerReq = Protocol.WorkerRequest(Job)
  module WorkerRes = Protocol.WorkerResponse(Job)
  
  (* local map_reduce for now. REPLACE LATER *)
  let map_reduce inputs =
    Deferred.List.map ~how:`Parallel inputs ~f:Job.map
      >>| List.flatten
      >>| C.combine
      >>= Deferred.List.map ~how:`Parallel ~f:reduce

end

