(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* September 18, 2014 *)

(* PROBLEM ONE *)

type 'a exprTree =
	| Val of 'a
	| Unop of ( ' a -> 'a ) * 'a exprTree
	| Binop of ( ' a -> 'a -> 'a ) * 'a exprTree * 'a exprTree

(* Exercise 1:
 * Counts the number of operations in an expression tree
 * requires: 'a exprTree
 * returns: int *)
let rec count_ops = function
	| Val(a) -> 0
	| Unop(_, a) -> 1 + (count_ops a)
	| Binop(_, a, b) -> 1 + (count_ops a) + (count_ops b)

