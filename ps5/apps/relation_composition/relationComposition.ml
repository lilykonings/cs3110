open Async.Std
open AppUtils

type relation = R | S

module Job = struct
  type input  = relation * string * string
  type key    = string
  type inter  = (relation * string)
  type output = (string * string) list

  exception AppFailure of string 

  let name = "composition.job"

  (* creates a list of (key, inter) tuples. The key is determined by [r]
   * and the inter contents is also determined by [r]. The returned
   * list should contain only one element. 
   * requires: an input type argument 
   * returns: an expression of type (key * inter) list Deferred.t *)
  let map (r, x, y : input) : (key * inter) list Deferred.t =
    match r with 
    | R -> return ((y, (R,x))::[])
    | S -> return ((x, (S,y))::[])

  (* creates a list of all possible combinations of the pairs of elements
    that are found in the relations for a given key.  
   * requires: a (key*inter list) type argument
   * returns: an output Deferred.t *)
  let reduce (_, vs : key * inter list) : output Deferred.t =
    let (xs,zs) = List.fold_left (fun (xs,zs) (r,elem) -> 
      match r with 
      | R -> (elem::xs, zs) 
      | S -> (xs, elem::zs)) ([],[]) vs in 

    let lst = List.map (fun x -> 
      List.fold_left (fun acc z -> (x,z)::acc) [] zs) xs in

    (*List.flatten is NOT tail recursive *)
     return (List.flatten lst)

end

let () = MapReduce.register_job (module Job)

let read_line (line: string) : (string * string) =
  match Str.split (Str.regexp ",") line with
    | [domain; range] -> (String.trim domain, String.trim range)
    | _ -> failwith "Malformed input in relation file."

let read_file (r: relation) (file: string) : (relation * string * string) list Deferred.t =
      Reader.file_lines file            >>= fun lines  ->
      return (List.map read_line lines) >>= fun result ->
      return (List.map (fun (domain, range) -> (r, domain, range)) result)

module App = struct
  let name = "composition"

  let clean_and_print vs =
    List.map snd vs   |>
    List.flatten      |>
    List.sort compare |>
    List.iter (fun (a, b) -> printf "(%s, %s)\n" a b)

  module Make (Controller : MapReduce.Controller) = struct
    module MR = Controller(Job)

    (* You may assume that main is called with two valid relation files. You do
       not need to handle malformed input. For example relation files, see the
       data directory. *)
    let main args =
      match args with
      | [rfile; sfile] -> begin
          read_file R rfile >>= fun r ->
          read_file S sfile >>= fun s ->
          return (r @ s)
          >>= MR.map_reduce
          >>| clean_and_print
      end
      | _ -> failwith "Incorrect number of input files. Please provide two files."
  end
end

let () = MapReduce.register_app (module App)
