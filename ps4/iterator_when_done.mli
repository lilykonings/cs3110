module type ITERATOR =
  sig
    type 'a t
    exception NoResult
    val has_next : 'a t -> bool
    val next : 'a t -> 'a
  end
module type LIST_ITERATOR =
  sig
    type 'a t
    exception NoResult
    val has_next : 'a t -> bool
    val next : 'a t -> 'a
    val create : 'a list -> 'a t
  end
module ListIterator : LIST_ITERATOR
type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)
module type INORDER_TREE_ITERATOR =
  sig
    type 'a t
    exception NoResult
    val has_next : 'a t -> bool
    val next : 'a t -> 'a
    val create : 'a tree -> 'a t
  end
module InorderTreeIterator : INORDER_TREE_ITERATOR
module type TAKE_ITERATOR =
  functor (I : ITERATOR) ->
    sig
      type 'a t
      exception NoResult
      val has_next : 'a t -> bool
      val next : 'a t -> 'a
      val create : int -> 'a I.t -> 'a t
    end
module TakeIterator : TAKE_ITERATOR
module IteratorUtilsFn :
  functor (I : ITERATOR) ->
    sig
      val advance : int -> 'a I.t -> unit
      val fold : ('a -> 'b -> 'a) -> 'a -> 'b I.t -> 'a
    end
module type RANGE_ITERATOR =
  functor (I : ITERATOR) ->
    sig
      type 'a t
      exception NoResult
      val has_next : 'a t -> bool
      val next : 'a t -> 'a
      val create : int -> int -> 'a I.t -> 'a t
    end
module RangeIterator : RANGE_ITERATOR
