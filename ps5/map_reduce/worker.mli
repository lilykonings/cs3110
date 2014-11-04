(** The interface to the worker server. *)

open Async.Std

module Make (Job : MapReduce.Job) : sig
  (** Handle all of the requests for a single connection.  The Reader and
      Writer should be used to send and receive messages of type
      WorkerResponse(Job) and WorkerRequest(Job).

      [run] should return when all messages from the connection have been
      processed.  When the Deferred returned by run is determined, the
      connection will be closed. *)
  val run : Reader.t -> Writer.t -> unit Deferred.t
end

(** Start the worker server.  *)
val init : int -> unit Deferred.t

