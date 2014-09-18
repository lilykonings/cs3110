
(* PART 1: EXPRESSION TREES *)
type 'a exprTree = 
	| Val of 'a
	| Unop of ('a -> 'a) * 'a exprTree 
	| Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree 

(* Exercise 1 *)
(*returns the number of function app in an exprTree*)
(*requires: 'a exprTree*)
(*returns: int*)
let rec count_ops (et : 'a exprTree) : int = function
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

(* Exercise 1:
 * Returns the product of elements of a float list. 
 * The product of an empty list is 1.0.
 * requires: float list
 * returns: float of product *)
let product (lst : float list) : float =
  List.fold_left (fun acc x -> acc *. x) 1.0 lst

(* Exercise 2:
 * Returns in-order concatenation of all strings in input list. 
 * requires: string list
 * returns: string concatenation of string elements *)
let concat_left (lst : string list) : string = 
  List.fold_left (fun acc x -> acc ^ x) "" lst

let concat_right (lst : string list) : string = 
  List.fold_right (fun x acc -> x ^ acc) lst ""

(* Exercise 3a:
 * Performs function f on lst where the index is the first argument
 * and the element is the second.
 * requires: int -> 'a -> 'b function, and 'a list
 * returns: 'b list *)
let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  List.fold_left ((fun f acc x -> acc@[f (List.length acc) x]) f) [] lst

(* Exercise 3b:
 * Produces numbered outline from strings.
 * requires: string list
 * returns: string list with number, period and space prepended to each element *)
let outline (lst: string list) : string list =
  mapi_lst (fun i a -> (string_of_int (i + 1)) ^ ". " ^ a) lst

(* Exercise 4a: 
 * Returns list for each value taken by the accumulator during processing fold.
 * requires: function 'a -> 'b -> 'b, 'a list, and accumulator 'b
 * returns: 'b list of all accumulator values taken *)      
let scan_right (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b list =
  List.rev (List.fold_right ((fun f x a -> (f x (List.hd a))::a) f) lst [acc])

(* Returns list for each value taken by the accumulator during processing fold.
 * requires: function 'a -> 'b -> 'a, accumulator 'a, and 'b list
 * returns: 'a list of all accumulator values taken *)        
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  List.rev (List.fold_left ((fun f a x -> (f (List.hd a) x)::a) f) [acc] lst)

(* requires: n >= 1 
   returns: the list [1;2;...;n] *)
let countup (n:int) : int list =
  (* tail-recursive helper function for countup:  
       accumulate the answer in l, 
       starting from n and working down *)
  let rec countup' i l =
    if i<=0 then l
    else countup' (i-1) (i::l)
  in countup' n []

(* Exercise 4b:
 * Returns an int list containing the factorial for all numbers up to an int  n
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
	let insert e lst =
		List.fold_right (fun a x -> if (x=[]) then a::e::x else a::x) lst [] in
	try 
		List.fold_left2 (fun a x1 x2 -> List.rev((insert x1 x2)::a)) [] c m
	with
		Invalid_argument _ -> raise (MatrixFailure "Sizes of matrix and vector do not match!")

(* Exercise 3 *)
(*transposes a matrix*)
(*requires: matrix*)
(*returns: matrix *)
let transpose (m : matrix) : matrix = 

(* Exercise 4 *)
(*adds two matrices*)
(*requires: two matrices*)
(*returns: matrix *)
let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
	let add l1 l2 =
    List.fold_left2 (fun a x1 x2 -> ((x1+x2)::a)) [] l1 l2 in
  List.fold_left2 (fun a x1 x2 -> List.rev(List.rev(add x1 x2)::a)) [] m1 m2

(* Exercise 5 *)
(*multiplies two matrices*)
(*requires: two matrices*)
(*returns: matrix *)
let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix = 

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
	failwith "Listen Ness. I'm going to tell you something very important. You may
	want to take notes. Ready? ......You're the chosen one." 

																					(*-Talking Rock, in the Underworld*)

(*counts the number of wildcards in a pattern, and adds that quantity to the sum
of the lengths of the variable names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
	failwith "I have fake teeth, so I like soft foods. Not like rocks or stones. 
	They're too hard." 

																					(*-Old Lady at Summer's Restaurant*)

(*counts how oftern a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
	failwith "Kidnapping is wrong! I'll be careful not to kidnap anyone!"

												 (*- Mr T guy in front of Department Store in Twoson*)

(*2. *************************************************************************)

let rec extract_names (p: pat) : string list = 
	failwith "We offer a special discount on tombstones for those that have passed
	away in our hospital."

																											(*-Onett Hospital Sign*)

let has_dups (l: 'a list) : bool = 
	failwith "If you stay here too long, you'll end up frying your brain. Yes, you
	will. No, you will...not. Yesno you will won't." 

																												 (*- Guy in Moonside*)

let all_vars_unique (p: pat) : bool = 
	failwith "I've come up with another wacky invention that I think has real 
	potential. Maybe you won't, but anyway...It's called the 'Gourmet Yogurt 
	Machine.' It makes many different flavors of yogurt. The only problem is, 
	right now, it can only make trout-flavored yogurt..."

																																(*-Apple Kid*)
(*3. *************************************************************************)

let all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =
	failwith "Oh yes, yes. My co-worker, Big Foot, dislikes violence. He's such a 
	nice guy, and he loves people. He often shares his beef jerky with me..." 

																														 (*-Dr. Andonuts*)

(*4. *************************************************************************)

let rec match_pat ((v:value),(p:pat)) : bindings =
	failwith "If they break their contract, they'll be in deep doodoo with the 
	police. The police would probably say, 'Hey you guys!' or something like 
	that..." 

																									(*-Topolla Theater Manager*)

(*5. *************************************************************************)
exception NoAnswer

let rec first_answer (f: 'a -> 'b option) (l: 'a list) : 'b =
	failwith "Didactically speaking, seminal evidence seems to explicate the fact
	that your repudiation of entropy supports my theory of space-time synthesis. 
	Of this, I am irrefutably confident." 

																							(*-Wordy guy at the Stoic Club*)

(*6. *************************************************************************)

let match_pats ((v: value), (ps: pat list)) : bindings =
	failwith "My dad really got after me. He said I get no dessert for the rest of
	the decade..." 

																																		(*-Pokey*) 
