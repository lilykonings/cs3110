open Async.Std

(******************************************************************************)
(** controller                                                                *)
(******************************************************************************)

module Make (Job : MapReduce.Job) = struct

  let reduce (k, vs) =
    Job.reduce (k, vs) >>| fun out -> (k, out)

  module C = Combiner.Make(Job)

  let map_reduce inputs =
    Deferred.List.map ~how:`Parallel inputs ~f:Job.map
      >>| List.flatten
      >>| C.combine
      >>= Deferred.List.map ~how:`Parallel ~f:reduce

end
