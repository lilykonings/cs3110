type coord = float * float
type region = coord * coord
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree *
            'a quadtree
| Leaf of region * (coord * 'a) list
val min_diagonal : float
exception OutOfBounds
val new_tree : region -> 'a quadtree
val insert : 'a quadtree -> coord -> 'a -> 'a quadtree
val fold_quad : ('a -> coord * 'b -> 'a) -> 'a -> 'b quadtree -> 'a
val fold_region :
  ('a -> coord * 'b -> 'a) -> 'a -> 'b quadtree -> region-> 'a

