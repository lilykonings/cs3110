(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 2, 2014 *)

open Parser
open Quadtree

(* inserts all the city names from a file into a quadtree by
 * latitude and longtitude *)
(* requires: the string name of the file *)
(* returns: the resulting string quadtree of city names *)
let load_city_data (s:string) : string quadtree =
	List.fold_left (fun acc x ->
		let (x,y,name) = x in
		insert acc (x,y) name
	) (new_tree (((-90.),(-180.)),(90.,180.))) (parse s)

(* returns all of the cities within a given region *)
(* requires: string quadtree to be searched and the region with
 * which the search will be narrowed down by *)
(* returns: a string list of all the names of the cities that
 * were found inside the region *)
let city_search (q: string quadtree) (r: region) : string list = 
	fold_region (fun acc x -> (snd x)::acc) [] q r
