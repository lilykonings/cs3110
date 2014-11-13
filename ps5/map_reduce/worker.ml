open Async.Std

module Make (Job : MapReduce.Job) = struct
    module Request = Protocol.WorkerRequest (Job)
    module Response = Protocol.WorkerResponse (Job)

  (* see .mli *)
  let run (r: Reader.t) (w: Writer.t) : unit Deferred.t =
    let handle (job_result: 'a Deferred.t) =
      try_with (fun () -> job_result) 
        >>| function
          | Core.Std.Result.Ok (a) -> (match a with 
            | [] as lst -> Response.send w (Response.MapResult lst)
            | (key,inter) :: t as lst -> Response.send w (Response.MapResult lst)) 
          | Core.Std.Result.Error _ -> Response.send w (Response.JobFailed "Job failed") in 
    Request.receive r >>= function
      | `Ok job_request -> (match job_request with
        | Request.MapRequest input -> handle (Job.map input)
        | Request.ReduceRequest (key,inters) ->
          begin
            try_with (fun () -> Job.reduce (key,inters))
            >>| function
              | Core.Std.Result.Ok result -> Response.send w (Response.ReduceResult result)
              | Core.Std.Result.Error _ -> Response.send w (Response.JobFailed "Job failed")
          end)
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


