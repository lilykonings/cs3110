open Async.Std

(******************************************************************************)
(* types from .mli                                                            *)
(******************************************************************************)
type id = string

module type Job = sig
  type input
  type key
  type inter
  type output

  val name   : id
  val map    : input -> (key * inter) list Deferred.t
  val reduce : (key * inter list) -> output Deferred.t
end

module type Controller = functor (Job : Job) -> sig
  val map_reduce : Job.input list -> (Job.key * Job.output) list Deferred.t
end

module type EntryPoint = sig
  val main : string list -> unit Deferred.t
end

module type App = sig
  val name : id
  module Make (C : Controller) : EntryPoint
end

(* Note: this code is duplicated below *)
let job_table = Hashtbl.create 8

let register_job job =
  let (module J : Job) = job in
  if Hashtbl.mem job_table J.name
    then failwith ("duplicate registration: " ^ J.name)
    else Hashtbl.add job_table J.name job

let get_job name =
  if Hashtbl.mem job_table name
    then Some (Hashtbl.find job_table name)
    else None

(* Note: this code is duplicated above *)
let list_jobs () =
  Hashtbl.fold (fun k _ a -> k::a) job_table []

let app_table = Hashtbl.create 8

let register_app app =
  let (module A : App) = app in
  if Hashtbl.mem app_table A.name
    then failwith ("duplicate registration: " ^ A.name)
    else Hashtbl.add app_table A.name app

let get_app name =
  if Hashtbl.mem app_table name
    then Some (Hashtbl.find app_table name)
    else None

let list_apps () =
  Hashtbl.fold (fun k _ a -> k::a) app_table []


