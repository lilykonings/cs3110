(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* September 18, 2014 *)

(* PART 1: EXPRESSION TREES *)
type 'a exprTree = 
	| Val of 'a
	| Unop of ('a -> 'a) * 'a exprTree 
	| Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree 

(* Exercise 1 *)
(*returns the number of function app in an exprTree*)
(*requires: 'a exprTree*)
(*returns: int*)
let rec count_ops = function
	| Val(a) -> 0
	| Unop(_, a) -> 1 + (count_ops a)
	| Binop(_, a, b) -> 1 + (count_ops a) + (count_ops b)
	
(* Exercise 2 *)
(*returns an exprTree representing the execution of fact n (as defined in the
	write-up for exercise 5)*)
(*requires: int*)
(*returns: int exprTree*)
let rec make_fact_tree (n : int) : int exprTree =
	if (n<=0) then Val(1)
	else Binop(( * ), Val(n), make_fact_tree (n-1))
	
(* Exercise 3 *)
(*computes the expression represented by [et]*)
(*requires: 'a exprTree*)
(*returns: 'a*)
let rec eval : 'a = function
	| Val(a) -> a
	| Unop(f, a) -> f (eval a)
	| Binop(f, a, b) -> f (eval a) (eval b)

(* PART 2: FOLDING*)

(* Exercise 1*)
(*returns the product of elements of a float list*)
(*the product of an empty list is 1.0*)
(*requires: float list*)
(*returns: float of product *)
let product (lst : float list) : float =
  List.fold_left (fun acc x -> acc *. x) 1.0 lst

(* Exercise 2 *)
(*returns in-order concatenation of all strings in input list*)
(*requires: string list*)
(*returns: string concatenation of string elements *)
let concat_left (lst : string list) : string = 
  List.fold_left (fun acc x -> acc ^ x) "" lst

let concat_right (lst : string list) : string = 
  List.fold_right (fun x acc -> x ^ acc) lst ""

(* Exercise 3a *)
(*performs function f on lst where the index is the first argument
  and the element is the second*)
(*requires: int -> 'a -> 'b function, and 'a list*)
(*returns: 'b list*)
let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  List.fold_left ((fun f acc x -> acc@[f (List.length acc) x]) f) [] lst

(* Exercise 3b *)
(*produces numbered outline from strings*)
(*requires: string list*)
(*returns: string list with number, period and space prepended to each element*)
let outline (lst: string list) : string list =
  mapi_lst (fun i a -> (string_of_int (i + 1)) ^ ". " ^ a) lst

(* Exercise 4a *)
(*returns list for each value taken by the accumulator during processing fold.*)
(*requires: function 'a -> 'b -> 'b, 'a list, and accumulator 'b*)
(*returns: 'b list of all accumulator values taken*)      
let scan_right (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b list =
  List.rev (List.fold_right ((fun f x a -> (f x (List.hd a))::a) f) lst [acc])

(*returns list for each value taken by the accumulator during processing fold.*)
(*requires: function 'a -> 'b -> 'a, accumulator 'a, and 'b list*)
(*returns: 'a list of all accumulator values taken*)        
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  List.rev (List.fold_left ((fun f a x -> (f (List.hd a) x)::a) f) [acc] lst)

(* Exercise 4b *)
(* Creates a list that contains integers up to and including n *)
(*requires: n >= 1*)
(*returns: the list [1;2;...;n]*)
let countup (n:int) : int list =
  (* tail-recursive helper function for countup:  
  	accumulate the answer in l, 
  	starting from n and working down *)
  let rec countup' i l =
    if i<=0 then l
    else countup' (i-1) (i::l)
  in countup' n []

(* Exercise 4b:
 * Returns an int list containing the factorial for all numbers up to an int n
 * requires: int n >= 1
 * returns: an int list [1!; 2!; ... ; (n-1)!; n!] *)
let fact_list (n: int) : int list =
  List.tl (scan_left (fun x y -> x * y) 1 (countup n))

(* PART 3: MATRICES *)
type vector = int list
type matrix = vector list

exception MatrixFailure of string

(* Exercise 1 *)
(*prints a matrix*)
(*requires: matrix*)
(*returns: unit*)
let show (m : matrix) : unit = 
	let show_helper lst =
		let print_list = 
			List.fold_left (fun a x -> (print_int x); (print_string " "); a) () in
		print_list lst;
		print_string "\n" in
	List.fold_left (fun a x -> (show_helper x); a) () m

(* Exercise 2 *)
(*takes a matrix and inserts a vector into its right-most column*)
(*requires: matrix and vector*)
(*returns: matrix *)
let insert_col (m : matrix) (c : vector) : matrix = 
	try
		List.rev (List.fold_left2 (fun a x1 x2 -> (x1 @ [x2])::a) [] m c)
	with
		Invalid_argument _ -> raise (MatrixFailure "Sizes of matrix and vector do not match!")

(* Exercise 3 *)
(*transposes a matrix*)
(*requires: matrix*)
(*returns: matrix *)
let transpose (m : matrix) : matrix = 
  let base = function
    | [] -> []
    | h::_ -> List.fold_left (fun a x -> []::a) [] h in
	List.fold_left (fun a x -> insert_col a x) (base m) m

(* Exercise 4 *)
(*adds two matrices*)
(*requires: two matrices*)
(*returns: matrix *)
let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
	try
		List.map2 (fun x1 x2 ->
			List.rev (List.fold_left2 (fun a i1 i2 ->
				((i1+i2)::a)
			) [] x1 x2)
		) m1 m2
	with
		Invalid_argument _ -> raise (MatrixFailure "Sizes of matrix and vector do not match!")

(* Exercise 5 *)
(*multiplies two matrices*)
(*requires: two matrices*)
(*returns: matrix *)
let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix =
	try
		List.map ((fun x1 ->
			List.map (fun x2 ->
				List.fold_left (+) 0 (List.map2 ( * ) x1 x2)
			) (transpose m2))
		) m1
	with
		Invalid_argument _ -> raise (MatrixFailure "Sizes of matrix and vector do not match!")

(* PART 4: PATTERN MATCHING *)

(*type definitions: **********************************************************)
type pat =
	| WCPat (*short for "wildcard pattern"; equivalent to an underscore*)
	| VarPat of string
	| UnitPat
	| ConstPat of int
	| TuplePat of pat list
	| StructorPat of string * (pat option) (*Short for "constructor pattern"*)

type value = 
	| ConstVal of int
	| UnitVal
	| TupleVal of value list
	| StructorVal of string * (value option)

type bindings = (string * value) list option

(*1. *************************************************************************)

let rec z f1 f2 p =
	let r = z f1 f2 in
		match p with
		| WCPat -> f1 ()
		| VarPat x -> f2 x
		| TuplePat ps -> List.fold_left (fun acc e -> (r e) + acc) 0 ps
		| StructorPat (_,Some p) -> r p
		| _ -> 0

(*counts the number of wildcards that occur in a pattern*)
let count_wcs (p: pat) : int =
	z (fun u -> 1) (fun s -> 0) p

(*counts the number of wildcards in a pattern, and adds that quantity to the sum
of the lengths of the variable names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
	z (fun u -> 1) (fun s -> String.length(s)) p

(*counts how oftern a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
	z (fun u -> 0) (fun s -> if (s = var_name) then 1 else 0) p

(*2. *************************************************************************)

(* Exercise 2: 
 * Produces list of variable names occurring in pattern, includes copies 
 * requires: pat p
 * returns: string list of variable names *)
let rec extract_names (p: pat) : string list = 
  match p with 
  	WCPat -> []
	| VarPat vs -> [vs]
	| UnitPat -> []
	| ConstPat i -> []
	| TuplePat tp -> List.fold_right (fun x acc -> (extract_names x)@acc) tp []
	| StructorPat (s, Some p) -> extract_names p
	| _ -> []

(* Checks whether duplicates exist in a list of values
 * requires: 'a list
 * returns: true if duplicates exist, false otherwise *)
let rec has_dups (l: 'a list) : bool = 
  match l with
  [] -> false
  | h::[] -> false
  | h::t1::t2 -> if (h = t1) then true else has_dups (h::t2) || has_dups (t1::t2)

(* Determines whether all variables in the pattern have unique names.
 * requires: pat p
 * returns: true if unique list of names, false otherwise *)
let all_vars_unique (p: pat) : bool = 
	(has_dups (extract_names p)) = false

(*3. *************************************************************************)

(*applies the function argument to every element of the list argument*)
(*requires: function: 'a -> 'b list option and list: 'a list*)
(*returns: 'b list option*)
let all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =
	let rec no_none = function
		| [] -> true
		| h::t -> if h = None then false else (no_none t) in
	let r = List.map f l in
	if (no_none r = false) then None
	else let no_option = function
		| None -> []
		| Some e -> e in
		Some (List.concat (List.map no_option r))
		

(*4. *************************************************************************)

let rec match_pat ((v:value),(p:pat)) : bindings =
	match p,v with
		| WCPat, _ -> Some []
		| VarPat s, (_ as x) -> Some [(s,x)]
		| UnitPat, UnitVal -> Some []
		| ConstPat x1, ConstVal x2 -> Some []
		| TuplePat pats, TupleVal vals ->
			if (List.length pats = List.length vals) then
				all_answers match_pat (List.fold_left2 (fun a x1 x2 -> (x1,x2)::a) [] vals pats)
			else None
		| StructorPat (s1, Some pats), StructorVal (s2, Some vals) ->
			match_pat ((vals,pats))
		| _, _ -> None

(*5. *************************************************************************)
exception NoAnswer

(* Exercise 5: 
 * Applies function f to elements of l until it returns Some v, in which case,
 * first_answer will return v.
 * requires: function 'a -> 'b option and l of type 'a list
 * returns: value of type 'b *)
let rec first_answer (f: 'a -> 'b option) (l: 'a list) : 'b =
  match l with
  [] -> raise NoAnswer
  | h::t -> match f h with
  	Some v -> v
  	| _ -> first_answer f t

(*6. *************************************************************************)

(* Exercise 6:
 * Checks whether v matches any of the patterns in ps 
 * requires: v of type value, ps of type pat list 
 * returns: bindings *)
let rec match_pats ((v: value), (ps: pat list)) : bindings =
  match ps with 
  [] -> None
  | h::t -> match (match_pat (v,h)) with
  	None -> match_pats v t
	| Some b -> Some b

(*
let match_pats2 ((v: value), (ps: pat list)) : bindings =
  Some (first_answer (match_pat v) ps)
*)
