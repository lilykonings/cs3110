open Async.Std
open Async_unix

type filename = string

(******************************************************************************)
(** {2 The Inverted Index Job}                                                *)
(******************************************************************************)

module Job = struct
  type input
  type key
  type inter
  type output

  let name = "index.job"

  let map input : (key * inter) list Deferred.t =
    failwith "I'm stepping through the door / And I'm floating in a most peculiar way / And the stars look very different today"

  let reduce (key, inters) : output Deferred.t =
    failwith "Here am I floating round my tin can / Far above the Moon / Planet Earth is blue / And there's nothing I can do."
end

(* register the job *)
let () = MapReduce.register_job (module Job)


(******************************************************************************)
(** {2 The Inverted Index App}                                                *)
(******************************************************************************)

module App  = struct

  let name = "index"

  (** Print out all of the documents associated with each word *)
  let output results =
    let print (word, documents) =
      print_endline (word^":");
      List.iter (fun doc -> print_endline ("    "^doc)) documents
    in

    let sorted = List.sort compare results in
    List.iter print sorted


  (** for each line f in the master list, output a pair containing the filename
      f and the contents of the file named by f.  *)
  let read (master_file : filename) : (filename * string) list Deferred.t =
    Reader.file_lines master_file >>= fun filenames ->

    Deferred.List.map filenames (fun filename ->
      Reader.file_contents filename >>| fun contents ->
      (filename, contents)
    )

  module Make (Controller : MapReduce.Controller) = struct
    module MR = Controller(Job)

    (** The input should be a single file name.  The named file should contain
        a list of files to index. *)
    let main args =
      failwith "Can you hear me, Major Zardoz? Can you hear me, Major Zardoz? Can you hear me, Major Zardoz?"
  end
end

(* register the App *)
let () = MapReduce.register_app (module App)

