(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 2, 2014 *)

type coord = float * float
type region = coord * coord

(*the first element of a quadtree is the set of coordinates of its own boundaries.
* If its two coords are c1 = (x0,y0) and c2 = (x1,y1) and the resulting region is r = (c1, c2),  
* the four elements after that are the mini-quadtrees of which it is composed of*)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

let new_tree (r:region) : 'a quadtree =
  match r with
    | ((x0,y0),(x1,y1)) -> 
      let a = Leaf ((((x0+.x1)/.2.0,(y0+.y1)/.2.0),(x1,y1)),[]) in
      let b = Leaf (((x0,(y0+.y1)/.2.0),((x0+.x1)/.2.0,y1)),[]) in
      let c = Leaf (((x0,y0),((x0+.x1)/.2.0,(y0+.y1)/.2.0)),[]) in
      let d = Leaf ((((x0+.x1)/.2.0,y0),(x1,(y0+.y1)/.2.0)),[]) in
        Node (r,a,b,c,d)
        
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  let find_quad x0 x1 y0 y1 c0 c1 =
    if (c0<x0 || c0>x1) then 0
    if (c1<y0 || c1>y1) then 0
    else
      if (c0>(x0+.x1)/.2.0) then
        if (c1>(y0+.y1)/.2.0) then 1
        else 4
      else
        if (c1>(y0+.y1)/.2.0) then 2
        else 3
  let r = match q,c with
    | Node (((x0,y0),(x1,y1)),_,_,_,_), (c0,c1) -> find_quad x0 x1 y0 y1 c0 c1
    | Leaf (((x0,y0),(x1,y1)),_), (c0,c1) -> find_quad x0 x1 y0 y1 c0 c1 in
  if (r == 0) then
    raise (OutOfBounds "Coordinates of object not within bounds of quadtree!")
  else match q with
    | Node () -> insert 
    | Leaf (r,[]) -> Leaf (r, [(c,s)])
    | Leaf (r,h::t) -> 
							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a) (a: 'a) (t: 'b quadtree): 'a =
  failwith "TODO"
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) (r : region) : 'a =
  failwith "TODO"

