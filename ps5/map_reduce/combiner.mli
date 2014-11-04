(** Utilities for the combine phase of map reduce. *)

module Make (Job : MapReduce.Job) : sig

  (** Function for grouping output from the mappers by key *)
  val combine : (Job.key * Job.inter) list -> (Job.key * Job.inter list) list
end

