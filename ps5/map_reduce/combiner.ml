
module Make (Job : MapReduce.Job) = struct

  (******************************************************************************)
  (** combine                                                                   *)
  (******************************************************************************)
  
  module  M = Map.Make (struct
    type t = Job.key
    let compare = compare
  end)

  (** adds v to the list t[k] *)
  let append t (k,v) =
    let l = if M.mem k t then M.find k t else [] in
    M.add k (v::l) t

  let combine pairs =
    M.bindings (List.fold_left append M.empty pairs)

end
