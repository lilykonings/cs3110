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
 * Evaluates the final value of an expression tree
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

let m = [[1;2;3];[42;41;40]]

(* Exercise 1:
 * Prints a matrix
 * requires: matrix
 * returns: unit *)
let show (m:matrix) =
	let show_helper lst =
		let print_list = 
			List.fold_left (fun a x -> (print_int x); (print_string " "); a) () in
		print_list lst;
		print_string "\n" in
	List.fold_left (fun a x -> (show_helper x); a) () m

(* Exercise 2:
 * Takes a matrix and inserts a vector into its right-most column
 * requires: matrix and vector
 * returns: matrix *)
let insert_col (m:matrix) (c:vector) : matrix =
	let insert e lst =
		List.fold_right (fun a x -> if (x=[]) then a::e::x else a::x) lst [] in
	try 
		List.fold_left2 (fun a x1 x2 -> List.rev((insert x1 x2)::a)) [] c m
	with
		Invalid_argument _ -> raise (MatrixFailure "Sizes of matrix and vector do not match!")

(* Exercise 3:
 * Transposes a matrix
 * requires: matrix
 * returns: matrix *)
let transpose (m:matrix) : matrix =
	List.fold_left (fun a x -> (insert_col a x)::a) [] m

(* Exercise 4:
 * Adds two matrices
 * requires: two matrices
 * returns: matrix *)







