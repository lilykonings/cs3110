type 'a exprTree =
    Val of 'a
  | Unop of ('a -> 'a) * 'a exprTree
  | Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree
val count_ops : 'a exprTree -> int
val make_fact_tree : int -> int exprTree
val eval : 'a exprTree -> 'a
val product : float list -> float
val concat_left : string list -> string
val concat_right : string list -> string
val mapi_lst : (int -> 'a -> 'b) -> 'a list -> 'b list
val outline : string list -> string list
val scan_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b list
val scan_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
val countup : int -> int list
val fact_list : int -> int list
type vector = int list
type matrix = vector list
exception MatrixFailure of string
val show : matrix -> unit
val insert_col : matrix -> vector -> matrix
val transpose : matrix -> matrix
val add_matrices : matrix -> matrix -> matrix
val multiply_matrices : matrix -> matrix -> matrix
type pat =
    WCPat
  | VarPat of string
  | UnitPat
  | ConstPat of int
  | TuplePat of pat list
  | StructorPat of string * pat option
type value =
    ConstVal of int
  | UnitVal
  | TupleVal of value list
  | StructorVal of string * value option
type bindings = (string * value) list option
val z : (unit -> int) -> (string -> int) -> pat -> int
val count_wcs : pat -> int
val count_wcs_and_var_lengths : pat -> int
val count_var : string -> pat -> int
val extract_names : pat -> string list
val has_dups : 'a list -> bool
val all_vars_unique : pat -> bool
val all_answers : ('a -> 'b list option) -> 'a list -> 'b list option
val match_pat : value * pat -> bindings
exception NoAnswer
val first_answer : ('a -> 'b option) -> 'a list -> 'b
val match_pats : value * pat list -> bindings
