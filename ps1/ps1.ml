(* Exercise 1:
 * Checks if an integer list is monotonically increasing.
 * requires: int list
 * returns: bool - true if list is monotonically increasing, false otherwise *)
let rec is_mon_inc (lst:int list) =
	match lst with
	 | [] -> true
	 | _::[] -> true
 	 | h1::(h2::t) -> (h1 <= h2) && is_mon_inc (h2::t)

(* Exercise 2:
 * Checks if an integer list is unimodal.
 * requires: int list
 * returns: bool - true if list is unimodal, false otherwise *)
let rec is_mon_dec (lst: int list) = 
	is_mon_inc (List.rev lst)

let rec is_unimodal (lst: int list) = 
	if (is_mon_inc lst) || (is_mon_dec lst) then true else
		let rec decreasing lst last = (match lst with
		  h::t -> if h <= last then decreasing t h
			 	else false
		  | [] -> true) in
		let rec increasing lst last = (match lst with
		  h::t -> if h >= last then increasing t h
				else decreasing t h
		  | [] -> true) in 
		match lst with
		  h::t -> increasing t h
		  | _ -> true

(* Exercise 3:
 * Returns the set of all subsets of a given list
 * requires: 'a list
 * returns: 'a list list - the powerset of the given list *)
let rec powerset lst =
	match lst with
	| [] -> [[]]
	| h::t ->
		let p = powerset t in
			let rec powerset_helper funct lst =
				match lst with
				| [] -> []
				| h::t -> funct h::powerset_helper funct t in
					(powerset_helper (fun tail -> h::tail) p) @ p

(* Exercise 4:
 * Reverses the digits of an integer. The sign is unchanged
 * requires: int
 * returns: int - int whose digits are reversed from the given int *)
let rev_int i =
	let rec reverse i r =
		if i = 0 then r
		else reverse (i/10) ((10*r) + (i mod 10)) in
			if i<0 then -1*(reverse (-1*i) 0)
			else reverse i 0

(* Exercise 5:
 * Turns a list into a list of sublists, each of a given size.
The last sublist may be smaller than the given size.
 * requires: int - size of each sublist. 'a list - original flat list 
 * returns: 'a list list option - a list of sublists, each of a given size *)
let unflatten k lst =
	if k<=0 then None
	else let rec unflatten_helper i a b = function
  	| [] -> []
  	| [x] -> (List.rev (x::a))::b
  	| h::t ->
	  	if i<=1 then unflatten_helper k [] ((List.rev (h::a))::b) t
		  else unflatten_helper (i-1) (h::a) b t in
		  	Some(List.rev (unflatten_helper k [] [] lst))

(* Exercise 6:
 * Converts the input roman numeral into a integer.
 * requires: valid roman numeral
 * returns: integer corresponding to roman numeral *)
type numeral = I | V | X | L | C | D | M
type roman = numeral list

let rec int_of_roman (r : roman) : int =
	let int_of_numeral = function
	  | I -> 1
	  | V -> 5
	  | X -> 10
	  | L -> 50
	  | C -> 100
	  | D -> 500
	  | M -> 1000 in
	match r with
	  [] -> 0
	  | a::[] -> int_of_numeral a 
	  | h::(h2::t) -> if (int_of_numeral h) >= (int_of_numeral h2) then
	  (int_of_numeral h) + (int_of_roman (h2::t)) else
	  		(int_of_numeral h2) - (int_of_numeral h) + (int_of_roman t)