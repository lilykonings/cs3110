open Async.Std

module Job = struct
  type input  = string * string list
  type key    = (string*string) 
  type inter  = string list 
  type output = string list

  exception AppFailure of string

  let name = "friends.job"

  (* creates a list of pairs (key, friend list) tuples. A key is a pair 
   * consisting of [name] and an element of [friendlist]. The order of 
   * the pair values is alphabetized, e.g. ("B","A") becomes ("A","B"). 
   * Each key's friend list is the remaining [friendlist] which does not
   * contain any element found in the key. There is only one (key, friend list)
   * tuple for each element in [friendlist].  
   * requires: an input type argument
   * returns: a (key * inter) list Deferred.t *)
  let map (name, friendlist : input) : (key * inter) list Deferred.t =
    let remove x lst =
      List.filter (fun elem -> elem <> x) lst  in
    return (List.fold_left (fun acc elem -> 
      let lst = remove elem friendlist in 
      if elem < name then ((elem, name), lst) :: acc
      else 
        ((name,elem), remove elem lst) :: acc ) [] friendlist)

  (* creates a list of mutual friends of the elements given in the key. 
   * [friendlists] should only contain two lists. 
   * requires: a (key*inter list) type argument
   * returns: an output Deferred.t *)
  let reduce (_, friendlists : key * inter list) : output Deferred.t =
    match friendlists with
    | lst1::lst2::[] -> 
      return (List.fold_left (fun acc elem ->
        if List.mem elem lst2 then elem::acc
        else
          acc) [] lst1)
    | _ -> raise (AppFailure "Failed at reduce: invalid inter list")
    
end

let () = MapReduce.register_job (module Job)

let read_line (line:string) :(string * (string list)) =
  match Str.split (Str.regexp ":") line with
    | [person; friends] -> begin
      let friends = Str.split (Str.regexp ",") friends in
      let trimmed = List.map String.trim friends in
      (person, trimmed)
    end
    | _ -> failwith "Malformed input in graph file."

let read_files (files: string list) : ((string * (string list)) list) Deferred.t =
  match files with
  | []    -> failwith "No graph files provided."
  | files -> begin
    Deferred.List.map files Reader.file_lines
    >>| List.flatten
    >>| List.map read_line
  end

module App = struct
  let name = "friends"

  let print common_friends =
    let print_friends ((a, b), friends) =
      printf "(%s, %s): %s\n" a b (String.concat ", " friends)
    in
    List.iter print_friends common_friends

  module Make (Controller : MapReduce.Controller) = struct
    module MR = Controller(Job)

    (* You may assume that main is called with a single, valid graph file. You
       do not need to handle malformed input. For example graph files, see the
       data directory. *)
    let main args =
        read_files args
        >>= MR.map_reduce
        (* replace this failwith with print once you've figured out the key and
           inter types*)
        >>| print 
  end
end

let () = MapReduce.register_app (module App)
