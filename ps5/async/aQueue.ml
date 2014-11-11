open Async.Std

(* an asynchronous queue. 'a s can be pushed onto the queue; popping blocks
 * until the queue is non-empty and then returns an element *)
type 'a t = ('a Pipe.Reader.t) * ('a Pipe.Writer.t)

(* creates a new queue *)
(* require: unit *)
(* returns: an empty queue of type 'a t *)
let create () : 'a t = Pipe.create ()

(* add an element to the queue *)
(* require: a queue of type 'a t
						an element of type 'a to be pushed into the queue *)
(* returns: unit *)
let push (q : 'a t) (x : 'a) : unit =
  ignore (Pipe.write (snd q) x);
  ()

(* waits until an element becomes available, and then return it *)
(* require: a queue of type 'a t *)
(* returns: the first element in the queue of type 'a Deferred.t *)
let pop (q : 'a t) : 'a Deferred.t =
  (Pipe.read (fst q)) >>= (fun e ->
  	match e with
  	|`Ok a -> return a
    |`Eof -> failwith "queue is closed")