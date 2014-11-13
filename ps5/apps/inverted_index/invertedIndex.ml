open Async.Std
open Async_unix

type filename = string

(******************************************************************************)
(** {2 The Inverted Index Job}                                                *)
(******************************************************************************)

module Job = struct

  type input = filename * string 
  type key = string 
  type inter = filename
  type output = filename list 
  
  exception AppFailure of string  

  let name = "index.job"

  (* creates a list of non-duplicated elements found in [lst]
   * requires: a list of type 'a elements
   * returns: a list of type 'a elements *)
  let elim_duplicates (lst: 'a list) : 'a list = 
    List.fold_left (fun acc elem -> 
      if (List.mem elem acc) then acc
      else 
        elem :: acc) [] lst 

  (* takes an [input] of type (filename * contents) and creates a list of 
   * (index, [filename]) tuples where indices are words found in [contents]
   * requires: an input type argument 
   * returns: a (key * inter) list Deferred t*)
  let map (filename, contents : input) : (key * inter) list Deferred.t =

    let read_contents (contents: string) : string list =
      let lines_lst = Str.split (Str.regexp " \n") contents in       
      List.fold_left (fun acc line -> 
        elim_duplicates (elim_duplicates (AppUtils.split_words line) @ acc)) 
        [] lines_lst  in 

    return (List.map (fun elem -> elem,filename) (read_contents contents))

  (* creates a list of non-duplicated filenames 
   * requires: a (key*inter list) type argument
   * returns: an output Deferred.t *)
  let reduce (key, inters : key * inter list) : output Deferred.t =
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
  let read (master_file: filename) : (filename * string) list Deferred.t =
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

