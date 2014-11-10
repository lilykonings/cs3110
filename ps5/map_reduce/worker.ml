open Async.Std

module Make (Job : MapReduce.Job) = struct

  (* see .mli *)
  let run (r: Reader.t) (w: Writer.t) : unit Deferred.t =
    (*get request from controller*)
    let request = Protocol.WorkerRequest (Job) in 
    let response = Protocol.WorkerResponse (Job) in 

(*     let is_broken_connection w r = 
 *)
    let exception_handler (job_result: 'a Deferred.t) : unit Deferred.t =
      try_with (fun () -> job_result) >>| fun a -> function
      | `Ok (a) -> (function
        | (key,inter) :: t as lst-> response.send w (response.MapResult lst)
        | output -> response.send w (ReduceResult output))
      | `Err _exn -> response.send w (JobFailed "Job failed")   

    in 
    (*Note: include async version of try-with, 
     * if it fails, do response.send w (JobFailed of string)*)
    (*process request*)
    request.receive r >>= function
    | `Ok job_request -> (function 
      (*do job*)
      | MapRequest input -> exception_handler (Job.map input)
      | ReduceRequest (key,inters) -> 
        exception_handler (Job.reduce (key,inters))



      (* -> Job.map input 
        >>| fun result 
        (* not sure how to access t in response.t *)
          (*send response back *)
          -> let response.t = MapResult(result) in 
            response.send w (MapResult result)

          -> exception_handler (result) *)

     (*  | ReduceRequest (key,inters) -> Job.reduce (key,inters))
        >>| fun result 
          (*send response back *)
          -> let response.t = ReduceResult result in 
            response.send w (ReduceResult result)

          -> exception_handler (result) *)

    | `Eof -> return (Pipe.close w) (*close the pipe*)

end

(* see .mli *)
let init port =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.on_port port)
    (fun _ r w ->
      Reader.read_line r >>= function
        | `Eof    -> return ()
        | `Ok job -> match MapReduce.get_job job with
          | None -> return ()
          | Some j ->
            let module Job = (val j) in
            let module Worker = Make(Job) in
            Worker.run r w
    )
    >>= fun _ ->
  print_endline "server started";
  print_endline "worker started.";
  print_endline "registered jobs:";
  List.iter print_endline (MapReduce.list_jobs ());
  never ()


