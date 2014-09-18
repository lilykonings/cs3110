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

(* Exercise 1 *)
let product (lst : float list) : float =
	failwith "I was hiding under your porch because I love you!!"

(* Exercise 2 *)
let concat_left (lst : string list) : string = 
	failwith "SQUIRREL!!"

let concat_right (lst : string list) : string = 
	failwith "POINT!"

(* Exercise 3 *)
let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
	failwith "My name is Dug. I have just met you, and I love you."

let outline (lst: string list) : string list =
	failwith "I found the snipe!"

(* Exercise 4 *)
let scan_right (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b list =
	failwith "Kevin's a girl?"
			
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
	failwith "Now, you must wear the cone of shame."

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

let fact_list (n: int) : int list =
	failwith "I do not like the cone of shame."

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
