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

(* Exercise 2:
 * Creates a tree representation of a factorial function
 * requires: int
 * returns: int exprTree *)
let rec make_fact_tree i =
	if (i<=0) then Val(1)
	else Binop(( * ), Val(i), make_fact_tree (i-1))

(* Exercise 3:
 * 
 * requires: 'a exprTree
 * returns: 'a *)
let rec eval = function
	| Val(a) -> a
	| Unop(f, a) -> f (eval a)
	| Binop(f, a, b) -> f (eval a) (eval b)


(* PROBLEM THREE *)
type vector = int list
type matrix = vector list
exception MatrixFailure of string

(* Exercise 1:
 * 
 * requires: 
 * returns:  *)
let show m =
	








