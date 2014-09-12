(* Checks if the int list is monotonically increasing. *)
(* requires: integer list *)
(* returns: true if list is monotonically increasing, false otherwise *)
(* let rec is_mon_inc (lst:int list) =
	match lst with
	 | [] -> true
 	 | h::t -> match t with
 	  | [] -> true
 	  | h2::t2 -> (h <= h2) && is_mon_inc (t) *)

let rec is_mon_inc (lst:int list) =
	match lst with
	 | [] -> true
	 | _::[] -> true
 	 | h1::(h2::t) -> (h1 <= h2) && is_mon_inc (h2::t)

(* Exercise 2 *)
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

(* Exercise 3 *)
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

(* Exercise 4 *)
let rev_int i =
	let rec reverse i r =
		if i = 0 then r
		else reverse (i/10) ((10*r) + (i mod 10)) in
			if i<0 then -1*(reverse (-1*i) 0)
			else reverse i 0

(* Exercise 5 *)
let unflatten k lst =
	if k<=0 then None
	else let rec unflatten_helper i a b = function
  	| [] -> []
  	| [x] -> (x::a)::b
  	| h::t ->
	  	if i<=1 then unflatten_helper k [] ((h::a)::b) t
		  else unflatten_helper (i-1) (h::a) b t in
		  	Some(List.rev(unflatten_helper k [] [] lst))







