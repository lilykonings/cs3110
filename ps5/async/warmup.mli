open Async.Std

val fork : 'a Deferred.t -> ('a -> 'b Deferred.t)
                         -> ('a -> 'c Deferred.t) -> unit

val deferred_map : 'a list -> ('a -> 'b Deferred.t) -> 'b list Deferred.t

