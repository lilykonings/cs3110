open Async.Std

(* runs two concurrent functions on d when it becomes determined *)
(* requires: a deferred computation d
						 two functions that takes in the determined value of d *)
(* returns: unit *)
let fork (d : 'a Deferred.t) (f1 : 'a -> 'b Deferred.t)
	(f2 : 'a -> 'c Deferred.t) : unit =
		ignore (d >>= f1);
		ignore (d >>= f2);
		()

(* maps list through a given function,
 * computing each element simultaneously *)
(* requires: a list of type 'a elements
						 a function that takes
 * an 'a type argument and returns 'b Deferred.t *)
(* returns: 'b list Deferred.t, each element is the output of
 * f taking in the corresponding element in l *)
let deferred_map (l : 'a list) (f : 'a -> 'b Deferred.t)
	: 'b list Deferred.t =
		let helper d acc =
			d >>= (fun x ->
				acc >>= (fun a ->
					return (x::a)
				)
			) in
		List.fold_right helper (List.map f l) (return [])
