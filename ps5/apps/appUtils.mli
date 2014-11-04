(** Helpful utilities for implementing MapReduce Apps. *)

(** Separate the words in the given string using whitespace and punctuation *)
val split_words : string -> string list

(** Tail recursive version of List.append *)
val (@): 'a list -> 'a list -> 'a list
