(** A [Controller] implementation that farms work out over the network. *)
module Make : MapReduce.Controller

(** If all of the workers die, the [map_reduce] function should raise an
    [InfrastructureFailure] exception. *)
exception InfrastructureFailure

(** If the application raises an exception while executing the [map] or
    [reduce] functions, then the worker should return a [JobFailed] message.
    Upon receiving this message, the controller should cause [map_reduce] to
    raise a [MapFailure] or [ReduceFailure] exception. *)
exception MapFailure of string
exception ReduceFailure of string

(** set up the map reduce controller to connect the the provided worker addresses *)
val init : (string * int) list -> unit
