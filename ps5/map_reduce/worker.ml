open Async.Std

module Make (Job : MapReduce.Job) = struct

  (* see .mli *)
  let run (r: Reader.t) (w: Writer.t) : unit Deferred.t =
    (*get request from controller*)
    let module Request = Protocol.WorkerRequest (Job) in 
    let module Response = Protocol.WorkerResponse (Job) in 

  let handle (job_result: 'a Deferred.t) =
    let open Response in  
    try_with (fun () -> job_result) 
      >>| function
        | Core.Std.Result.Ok (a) -> (match a with 
          | [] as lst -> send w (MapResult lst)
          | (key,inter) :: t as lst -> send w (MapResult lst)) 
          (* | output -> send w (ReduceResult output)     *)
        | Core.Std.Result.Error _ -> send w (JobFailed "Job failed") in 

      let open Request in 
      let open Response in 
      Request.receive r 
        >>= function 
          | `Ok job_request -> (match job_request with  
            | Request.MapRequest input -> handle (Job.map input)
            | Request.ReduceRequest (key,inters) -> 
                try_with (fun () -> Job.reduce (key,inters))
                >>| function
                  | Core.Std.Result.Ok result -> Response.send w (Response.ReduceResult result)
                  | Core.Std.Result.Error _ -> Response.send w (JobFailed "Job failed")) 
          | `Eof -> return () 
  
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


