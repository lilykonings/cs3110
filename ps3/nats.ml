(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t

  (* identity element for addition: a+0 === a *)
  (* returns: same inputed natural number if added *)
  val zero : t

  (* identity element for multiplication: a*1 === a *)
  (* returns: same inputed natural number if multiplied *)
  val one : t

  (* addition operator
   * associative: (a+b)+c === a+(b+c)
   * communitive: a+b === b+a *)
  (* requires: two natural numbers to be added *)
  (* returns: the sum also of type t *)
  val ( + ) : t -> t -> t

  (* multiplication operator
   * associative: (a*b)*c === a*(b*c)
   * communitive: a*b === b*a
   * distributive property over addition: a*(b+c) === (a*b)+(a*c) *)
  (* requires: two natural numbers to be multiplied *)
  (* returns: the product also of type t *)
  val ( * ) : t -> t -> t 

  (* checks if a natural number is smaller than another
   * transitive: a < b, b < c, then a < c *)
  (* requires: two natural numbers to be compared *)
  (* returns: true if the first is smaller than the second, false otherwise *)
  val ( < ) : t -> t -> bool

  (* checks if two natural numbers are equal
   * reflexive: a === a
   * transitive: a === b, b === c, then a === c
   * symmetric: a === b, b === a *)
  (* requires: two natural numbers to be compared *)
  (* returns: true if they are equal, false otherwise *)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable

  (* converts a natural number to primitive int type *)
  (* requires: the natural number to be converted *)
  (* returns: the resulting int or an Unrepresentable error if
   * the natural number cannot become an int *)
  val int_of_nat: t -> int

  (* converts a primitive int type to a natural number *)
  (* requires: an int to be converted *)
  (* returns: the resulting natural number of an Unrepresentable
   * error if the int cannot become a natural number *)
  val nat_of_int: int -> t
end

module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end

type sign = Positive | Negative
let sign_int (n:int) : sign = 
  if n >= 0 then Positive else Negative
let sum_overflows (i1:int) (i2:int) : bool = 
  sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
(* END: DO NOT CHANGE THIS CODE *)

(* EXERCISE 2 *)
module IntNat (M: NATN) = struct
  type t = int
  let zero = 0
  let one = 1

  let ( + ) i1 i2 =
    if sum_overflows i1 i2 then
      raise Unrepresentable
    else (i1 + i2)

  let ( * ) i1 i2 =
    match sign_int i1,sign_int i2 with
    | Positive, Positive ->
      if (i1 > (max_int / i2)) then
        raise Unrepresentable
      else (i1 * i2)
    | Positive, Negative ->
      if (i2 < (max_int / i1)) then
        raise Unrepresentable
      else (i1 * i2)
    | Negative, Positive ->
      if (i1 < (max_int / i2)) then
        raise Unrepresentable
      else (i1 * i2)
    | Negative, Negative ->
      if ((i1 != 0) && (i2 < (max_int / i1))) then
        raise Unrepresentable
      else (i1 * i2)

  let ( < ) i1 i2 = i1 < i2

  let ( === ) i1 i2 = i1 = i2

  let int_of_nat n = n

  let nat_of_int n =
    match n with
    | Positive -> n
    | Negative -> raise Unrepresentable
end

(* EXERCISE 3 *)
module ListNat (M: NATN) = struct
  (* The list [a1; ...; an] represents the
   * natural number n. That is, the list lst represents
   * length(lst). The empty list represents 0. The values of * the list elements are irrelevant. *)
  type t = int list
  
  let zero = []
  let one = []
  let ( + ) l1 l2 =
    List.fold_left2 (fun acc x1 x2 ->
      if sum_overflows i1 i2 then
        raise Unrepresentable
      else (x1 + x2)::acc
    ) 0 l1 l2

  let ( * ) l1 l2 = List.fold_left2 (fun acc x1 x2 ->
    match sign_int x1,sign_int x2 with
      | Positive, Positive ->
        if (x1 > (max_int / x2)) then
          raise Unrepresentable
        else (x1 * x2)::acc
      | Positive, Negative ->
        if (x2 < (max_int / x1)) then
          raise Unrepresentable
        else (x1 * x2)::acc
      | Negative, Positive ->
        if (x1 < (max_int / x2)) then
          raise Unrepresentable
        else (x1 * x2)::acc
      | Negative, Negative ->
        if ((x1 != 0) && (x2 < (max_int / x1))) then
          raise Unrepresentable
        else (x1 * x2)::acc
    ) 0 l1 l2

  let ( < ) l1 l2 =
    List.fold_left2 (fun acc x1 x2 ->
      (x1 < x2) && acc
    ) 0 l1 l2

  let ( === ) l1 l2 =
    List.fold_left2 (fun acc x1 x2 ->
      (x1 = x2) && acc
    ) 0 l1 l2

  let int_of_nat l = List.length l

  let nat_of_int n =
    let rec helper n l =
      if n > 0 then helper (n-1) (n::l)
      else l in
    helper n []
end

(* EXERCISE 4 *)
module NatConvertFn(N: NATN) = struct
  (* let int_of_nat(n: N.t): int = 

  let nat_of_int(n: int): N.t = *) 
end

(* EXERCISE 5 *)
module AlienNatFn (M: AlienMapping): NATN = struct
  type t = M.aliensym list
  let nat t = List.fold_right ((+) (List.map (M.int_of_aliensym t)) 0)

  (* Here we interpret an aliensym list as the sum of the ints that it represents *)
  let zero = M.int_of_aliensym M.one
  let one = M.int_of_aliensym M.zero
  let ( + ) i1 i2 = NATN.( + ) ((nat i1) (nat i2))
  let ( * ) i1 i2 = NATN.( * ) ((nat i1) (nat i2))
end
