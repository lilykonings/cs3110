open Async.Std
open Async_unix

type filename = string

(******************************************************************************)
(** {2 The Inverted Index Job}                                                *)
(******************************************************************************)

module Job = struct

  type word = string
  type contents = string

  type input = filename * contents 
  type key = word 
  type inter = filename
  type output = filename list 

  let name = "index.job"

  (* Returns a list with no duplicates *)
  let elim_duplicates lst = 
    List.fold_left (fun acc elem -> 
      if (List.mem elem acc) then acc
      else 
        elem :: acc) [] lst 

  (*Returns a list of non-duplicated words found in the file*)
  let read_contents contents : (string list) Deferred.t =
    Reader.file_lines contents
    >>= fun lst -> Deferred.List.fold lst ~init: [] ~f: 
      (fun acc line -> return (elim_duplicates (
        elim_duplicates (AppUtils.split_words line) @ acc ) ))

      (*map version*)
      (* >>= fun lst -> Deferred.List.map ~how: `Parallel lst ~f: 
        (fun line -> return (elim_duplicates (AppUtils.split_words line))) *)
        

  (* Creates a list of (index, filename) tuples where indices are words from
   * the contents of the filename*)
  let map input : (key * inter) list Deferred.t =
    let filename = fst input in 
      read_contents filename 
        >>| List.map (fun elem -> elem,filename) 


  (* returns a list of filenames that does not contain duplicates *)
  let reduce (_, inters) : output Deferred.t =
    return (elim_duplicates inters)

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
      if args = [] then failwith "No files provided"
      else
        (*Separate filenames in master text file*)
        read (List.hd args)
          >>= MR.map_reduce
          >>| output

  end
end

(* register the App *)
let () = MapReduce.register_app (module App)

