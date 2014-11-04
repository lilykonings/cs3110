(** The messages sent between the worker and the controller. *)

open Async.Std

(******************************************************************************)
(** {2 Sending and receiving messages}                                        *)
(******************************************************************************)

(** Send or receive marshaled messages through Reader or Writer *)
module type Marshalable = sig
  (** the type of a message *)
  type t

  (** [receive] and [send] receive and send messages.  They will raise
      exceptions if the connection is broken or 
      there was some kind of I/O failure (e.g. if the connection was
      unexpectedly terminated). *)
  val receive : Reader.t -> [`Ok of t | `Eof]  Deferred.t
  val send    : Writer.t -> t -> unit
end

(******************************************************************************)
(** {2 Mapper/Controller protocol}                                            *)
(******************************************************************************)

(** Messages from the controller to the worker *)
module WorkerRequest (Job : MapReduce.Job) : sig
  type t =
    | MapRequest of Job.input
      (** run the map phase for an input *)

    | ReduceRequest of Job.key * Job.inter list
      (** process the values associated with the given key *)

  (** You can send and receive [WorkerRequest(J).t]s by calling
      [WorkerRequest(J).send] and [receive] respectively.  These functions are
      inherited from {!Marshalable}: *)
  include Marshalable with type t := t
end

(** Messages from the worker to the controller *)
module WorkerResponse (Job : MapReduce.Job) : sig
  type t =
    | JobFailed of string
      (** The application threw the given exception with stacktrace *)

    | MapResult of (Job.key * Job.inter) list
      (** successful map output *)

    | ReduceResult of Job.output
      (** successful reduce output *)

  (** You can send and receive [WorkerRequest(J).t]s by calling
      [WorkerRequest(J).send] and [receive] respectively.  These functions are
      inherited from {!Marshalable}: *)
  include Marshalable with type t := t
end

