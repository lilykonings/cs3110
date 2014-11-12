open Async.Std

(******************************************************************************)
(** {2 Marshaling and unmarshaling}                                           *)
(******************************************************************************)

(* see .mli *)
module type Marshalable = sig
  type t
  val receive : Reader.t -> t Reader.Read_result.t Deferred.t
  val send    : Writer.t -> t -> unit
end

(** Utility class for implementing the Marshalable interface *)
module Marshaller = struct
  let receive r = Reader.read_marshal r
  let send w v  = Writer.write_marshal w [] v
end

(******************************************************************************)
(** {2 protocol messages}                                                     *)
(******************************************************************************)

module WorkerRequest (Job : MapReduce.Job) = struct
  type t =
    | MapRequest of Job.input
    | ReduceRequest of Job.key * Job.inter list

  include Marshaller
end

module WorkerResponse (Job : MapReduce.Job) = struct
  type t =
    | JobFailed of string
    | MapResult of (Job.key * Job.inter) list
    | ReduceResult of Job.output

  include Marshaller
end

