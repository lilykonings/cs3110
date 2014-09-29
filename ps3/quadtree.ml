type coord = float * float
type region = coord * coord
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

let new_tree (r:region) : 'a quadtree = failwith "TODO"
        
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree 
  = failwith "TODO"
							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  =
  failwith "TODO"
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
=
  failwith "TODO"

