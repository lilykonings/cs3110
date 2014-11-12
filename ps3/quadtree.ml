(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 2, 2014 *)

type coord = float * float
type region = coord * coord

(*The first element of a quadtree is the set of coordinates of its own boundaries.
* If its two coords are c1 = (x0,y0) and c2 = (x1,y1) and the resulting region is r = (c1, c2),  
* the four elements after that are the mini-quadtrees of which it is composed of*)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

(* initializes a new tree based on a given region *)
(* requires: region r *)
(* returns: a leaf quadtree containing no objects *)
let new_tree (r: region) : 'a quadtree =
  Leaf (r,[])

(* HELPER FOR INSERT AND FOLD_REGION *)
(* finds in which quadrant a given coordinate belongs to *)
(* requires: the overall region to put the coordinate and
 * the coordinate itself *)
(* returns: an int, if 1-4 corresponds to the quadrant
 * the coordinate is found in OR if 0, then the coordinate
 * is outside the given region *)
let find_quad (r: region) (c: coord) : int =
  match r,c with ((x0,y0),(x1,y1)),(c0,c1) ->
    if (c0<x0 || c0>x1 || c1<y0 || c1>y1) then 0
    else
      if (c0>(x0+.x1)/.2.0) then
        if (c1>(y0+.y1)/.2.0) then 1
        else 4
      else
        if (c1>(y0+.y1)/.2.0) then 2
        else 3

(* inserts an object at a given coordinate in a quadtree *)
(* requires: the quadtree that the object will be inserted into,
 * the coordinate that the object will be inserted at,
 * and the object itself *)
(* returns: the root of the resulting quadtree containing the object
 * OR an OutOfBounds exception if trying to insert the object
 * outside the region of the given quadtree *)
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  let quadrant =
    match q with
      | Node (r,_,_,_,_) -> find_quad r c
      | Leaf (r,_) -> find_quad r c in
  if (quadrant = 0) then raise OutOfBounds
  else
    match q with
      | Node (r,q1,q2,q3,q4) ->
        if (quadrant = 1) then Node (r,insert q1 c s,q2,q3,q4)
        else if (quadrant = 2) then Node (r,q1,insert q2 c s,q3,q4)
        else if (quadrant = 3) then Node (r,q1,q2,insert q3 c s,q4)
        else if (quadrant = 4) then Node (r,q1,q2,q3,insert q4 c s)
        else raise OutOfBounds
      | Leaf (r,[]) -> Leaf (r,[(c,s)])
      | Leaf (((x0,y0),(x1,y1)) as r,l) ->
        if ((sqrt ((+.) ((x1-.x0) **2.0) ((y1-.y0) **2.0)) < min_diagonal) || (List.length l < 1))
          then Leaf (r,((c,s)::l))
        else
          let l1,l2,l3,l4 = List.fold_left (fun (l1,l2,l3,l4) e ->
            let bounds = find_quad r (fst e) in
            if (bounds = 1) then e::l1,l2,l3,l4
            else if (bounds = 2) then l1,e::l2,l3,l4
            else if (bounds = 3) then l1,l2,e::l3,l4
            else if (bounds = 4) then l1,l2,l3,e::l4
            else raise OutOfBounds (* just safeguards *)
          ) ([],[],[],[]) l in
          let q1 = Leaf ((((x0+.x1)/.2.0,(y0+.y1)/.2.0),(x1,y1)),l1) in
          let q2 = Leaf (((x0,(y0+.y1)/.2.0),((x0+.x1)/.2.0,y1)),l2) in
          let q3 = Leaf (((x0,y0),((x0+.x1)/.2.0,(y0+.y1)/.2.0)),l3) in
          let q4 = Leaf ((((x0+.x1)/.2.0,y0),(x1,(y0+.y1)/.2.0)),l4) in
          let bounds = find_quad r c in
          if (bounds = 1) then Node (r,insert q1 c s,q2,q3,q4)
          else if (bounds = 2) then Node (r,q1,insert q2 c s,q3,q4)
          else if (bounds = 3) then Node (r,q1,q2,insert q3 c s,q4)
          else if (bounds = 4) then Node (r,q1,q2,q3,insert q4 c s)
          else raise OutOfBounds (* just safeguards *)
							      
(* applies a function argument to every object in a given quadtree *)
(* requires: the function to be applied to each object, the starting
 * accumulator argument of type ’a, and the quadtree containing all
 * objects to be affected *)
(* returns: resulting accumulated object of type 'a *)
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a) (a: 'a) (t: 'b quadtree): 'a =
  match t with
    | Leaf (_,[]) -> a
    | Leaf (_,l) -> List.fold_left f a l
    | Node (_,t1,t2,t3,t4) ->
      fold_quad f (fold_quad f (fold_quad f (fold_quad f a t1) t2) t3) t4
	   
(* folds the function argument over the quadtree, but applying it only to
 * those objects that are within the region argument *)
(* requires: the function to be applied to each object, the starting
 * accumulator argument of type ’a, the quadtree containing some of the
 * objects to be affected, and the region in the quadtree that contains
 * all of the objects to be affected *)
(* returns: resulting accumulated object of type 'a *)
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) (r : region) : 'a =
  match t with
    | Leaf (_,l) ->
      (* iterates through objects in leaf, checking if each is in
       * region, if so, apply function argument *)
      let rec fold_region_helper f acc = function
        | [] -> acc
        | h::t ->
          if (find_quad r (fst h) = 0) then acc
          else fold_region_helper f (f acc h) t in
      fold_region_helper f a l
    | Node (_,t1,t2,t3,t4) ->
      fold_region f (fold_region f (fold_region f (fold_region f a t1 r) t2 r) t3 r) t4 r
