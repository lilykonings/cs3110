open Async.Std

(******************************************************************************)
(** {2 The WordCount Job}                                                     *)
(******************************************************************************)

module Job = struct
  type line     = string
  type word     = string
  type count    = int

  type input  = line
  type key    = word
  type inter  = count
  type output = count

  let name = "wc.job"

  module M = Map.Make(String)

  (** Adds one to the value of [key] in the [table] *)
  let increment table key =
    let count = if M.mem key table then M.find key table else 0 in
    M.add key (count + 1) table

  (** split the line into words and count each word *)
  let map line =
    let words  = AppUtils.split_words line in
    let counts = List.fold_left increment M.empty words in
    return (M.bindings counts)

  (** sum the counts *)
  let reduce (_, counts) =
    return (List.fold_left (+) 0 counts)
end

(* Register the Job *)
let () = MapReduce.register_job (module Job)


(******************************************************************************)
(** {2 The WordCount App}                                                     *)
(******************************************************************************)

module App  = struct
  let name = "wc"

  (** Print out the counts of [l], sorted by count *)
  let output l =
    let print (word,count) =
      printf "%-15s %i\n%!" ("\""^word^"\":") count
    in
    let sorted = List.sort (fun (_,v1) (_,v2) -> v1 - v2) l in
    List.iter print sorted


  module Make (Controller : MapReduce.Controller) = struct
    module MR = Controller(Job)

    (** args is interpreted as a list of filenames; the words in all listed
        files are counted. *)
    let main args =
      if args = [] then failwith "No files provided."
      else
        Deferred.List.map args Reader.file_lines
          >>| List.flatten
          >>= MR.map_reduce
          >>| output
  end
end

(* register the App *)
let () = MapReduce.register_app (module App)

