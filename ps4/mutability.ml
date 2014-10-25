(* Creates function f that acts as a counter, starting from n and adding k *)
(* each time it is called. The function created takes in a unit. *)
(* The ith time f is called, it returns n+(i-1)*k. *)
(* requires: n and k integers *)
(* returns: a function unit -> int which returns n on first call and n+k after *)
let count_up_from n k = 
  let c = ref n in
  fun () ->
    begin
      c := (!c) + k;
      (!c) - k
    end

(* Returns array of length n where all the elements equal f applied to its index *)
(* requires: function f (int -> 'a) and integer n. f must be pure *)
(* returns: 'a array of length n *)
let tabulate f n = 
  Array.init n f

(* Computes f (... (f (f a b1) b2) ...) bn *)
(* requires: f of type ('a -> 'b -> 'a), acc of type 'a, and xs of type 'b list *)
(* returns: value of type 'a *)
let fold_left_imp f acc xs = 
  let ls = ref xs and a = ref acc in
  while ((!ls) != []) do
    match (!ls) with 
      h::t -> 
        begin
          a := (f (!a) h);
          ls := t
        end
      | _ -> ();
  done;
  !a

type t = int
type u = string
let lst : t list = [1;2;3]
let lst2 : t list = [1;1;1]
(* Function that returns a value of type u given x of value t. *)
(* Uses side effects to invalidate the following theorem: *)
(* List.map f (List.rev xs) = List.rev (List.map f xs) *)
(* requires: x of type t *)
(* returns: value of type u *)
let y = ref 0
let zardoz (x:t) : u = 
  begin
    y := !y + 1;
    string_of_int (!y + x)
  end