(** The core MapReduce types --- Job, App, and Controller *)

open Async.Std

type id = string

(******************************************************************************)
(** {2 Job interface}                                                         *)
(******************************************************************************)

(** MapReduce jobs transform data using the MapReduce framework *)
module type Job = sig
  type input
  type key
  type inter
  type output

  (** a unique name for the job *)
  val name   : id

  (** perform the map phase.  May throw an exception, in which case the job as a
      whole should fail.  *)
  val map    : input -> (key * inter) list Deferred.t

  (** perform the reduce phase.  May throw an exception, in which case the job
      as a whole should fail. *)
  val reduce : (key * inter list) -> output Deferred.t
end

(** The following three functions are used by the framework to find the module
    corresponding to a Job.  For each module J of type Job, you must call {[
      register_job (module J)
    ]}
    so that the framework can find the module when needed. *)

val register_job  : (module Job) -> unit
val get_job       : id           -> (module Job) option
val list_jobs     : unit         -> id list

(******************************************************************************)
(** {2 Controller interface}                                                  *)
(******************************************************************************)

module type Controller = functor (Job : Job) -> sig
  (** Execute that map and reduce phase of Job on the given list of inputs.
      Returns the entire list of intermediate keys and the reduce outputs
      corresponding to those keys.

      Raises an exception if the Job's map or reduce function raises an
      exception.

      Raises an exception if the Controller becomes unable to contact any
      worker. *)
  val map_reduce : Job.input list -> (Job.key * Job.output) list Deferred.t
end

(******************************************************************************)
(** {2 MapReduce Apps}                                                        *)
(******************************************************************************)

module type EntryPoint = sig

  (** the entry point of an app *)
  val main : string list -> unit Deferred.t
end

module type App = sig

  (** a unique name for the app *)
  val name : id
  module Make (C : Controller) : EntryPoint
end

(** The following three functions are used by the framework to find the module
    corresponding to an App.  For each module A of type App, you must call {[
      register_job (module A)
    ]}
    so that the framework can find the module when needed.  We have done this
    in the starter code we've provided. *)

val register_app  : (module App) -> unit
val get_app       : id           -> (module App) option
val list_apps     : unit         -> id list

