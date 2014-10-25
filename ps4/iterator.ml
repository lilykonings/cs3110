(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 23, 2014 *)

module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end

module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end

module ListIterator : LIST_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  let has_next (r : 'a list ref) : bool =
    match !r with
      | [] -> false
      | _ -> true

  let next (r : 'a list ref) : 'a =
    match !r with
      | [] -> raise NoResult
      | h::t ->
        begin
          r := t;
          h
        end

  let create (l : 'a list) : 'a list ref = ref l
end

type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end

module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
  type 'a t = 'a tree ref
  exception NoResult

  let has_next (r : 'a tree ref) : bool =
    match !r with
      | Leaf -> false
      | Node _ -> true

  let rec next (r : 'a tree ref) : 'a =
    match !r with
      | Leaf -> raise NoResult
      | Node (v,Leaf,right) ->
        begin
          r := right;
          v
        end
      | Node (v,left,right) ->
        begin
          let r2 = ref left in
          let a = next r2 in
          r := Node(v,!r2,right);
          a
        end

  let create (t : 'a tree) : 'a tree ref = ref t
end

module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end

module TakeIterator : TAKE_ITERATOR =
  functor (I : ITERATOR) ->
    struct
      type 'a t = (int ref) * 'a I.t
      exception NoResult

      let has_next (r : (int ref) * 'a I.t) : bool =
        let (n,i) = r in
        if (!n < 1) then false
        else true

      let next (r : (int ref) * 'a I.t) : 'a =
        let (n,i) = r in
        if (!n < 1) then raise NoResult
        else
          n := !n - 1;
          I.next i

      let create (n : int) (i : 'a I.t) : (int ref) * 'a I.t =
        (ref n, i)
    end

module IteratorUtilsFn (I : ITERATOR) = struct
  open I

  (* effects: causes iter to yield n results, ignoring
   *   those results.  Raises NoResult if iter does.  *)
  let advance (n: int) (iter: 'a I.t) : unit =
    let rec loop n =
      if n > 0 then (next iter; loop (n - 1)) in
    loop n;
    ()

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by iter,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
    if (has_next iter = false) then acc
    else
      let e = next iter in
      fold f (f acc e) iter
end

module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * requires: n and m > 0 because the int corresponds
   *   to 1st, 2nd, etc call to next
   * returns: an iterator that behaves the way i would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end

module RangeIterator : RANGE_ITERATOR =
  functor (I : ITERATOR) ->
    struct
      type 'a t = (int ref) * 'a I.t
      exception NoResult

      let has_next (r : (int ref) * 'a I.t) : bool =
        let (n,i) = r in
        if (!n < 1) then false
        else true

      let next (r : (int ref) * 'a I.t) : 'a =
        let (n,i) = r in
        if (!n < 1) then raise NoResult
        else
          n := !n - 1;
          I.next i

      let create (n : int) (m : int) (i : 'a I.t) : (int ref) * 'a I.t =
        if (n > m || n < 1) then raise NoResult
        else
          begin
            let module UtilsRange = IteratorUtilsFn(I) in
            UtilsRange.advance (n-1) i;
            (ref (m-n+1),i)
          end
    end
