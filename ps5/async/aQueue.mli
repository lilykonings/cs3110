open Async.Std

(** An asynchronous queue. 'a s can be pushed onto the queue; popping blocks
    until the queue is non-empty and then returns an element. *)
type 'a t

(** Create a new queue *)
val create : unit -> 'a t

(** Add an element to the queue. *)
val push   : 'a t -> 'a -> unit

(** Wait until an element becomes available, and then return it.  *)
val pop    : 'a t -> 'a Deferred.t

