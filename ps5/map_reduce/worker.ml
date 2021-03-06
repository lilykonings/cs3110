
open Async.Std

module Make (Job : MapReduce.Job) = struct

    module Request = Protocol.WorkerRequest (Job)
    module Response = Protocol.WorkerResponse (Job)

  (* Handles job requests sent by the controller. [run] takes 
   * each WorkerRequest(Job) sent by the contoller and performs the
   * necessary Job.map or Job.reduce function. Once it finishes, it 
   * sends back a WorkerResponse(Job) to the controller. If Job.map or
   * Job.reduce raises an exception, it should return a JobFailed message.
   * The module keeps handling jobs until its reader returns an Eof. 
   *  
   * requires: a Reader.t and a Writer.t
   * returns: unit Deferred.t
   * side effects: sends a message of type WorkerResponse(Job) to 
   *                the controller *)
  let run r w =
    let rec handle () =
      Request.receive r >>= function
        | `Eof -> return ()
        | `Ok job_request -> (
          match job_request with
            | Request.MapRequest input ->
                ((try_with (fun () -> Job.map input))
                  >>| (function
                      | Core.Std.Result.Error _ ->
                          Response.send w (Response.JobFailed "Job failed")
                      | Core.Std.Result.Ok result ->
                          Response.send w (Response.MapResult result))
                )
                >>= (fun _ -> handle ())
            | Request.ReduceRequest (k,i) ->
                ((try_with (fun () -> Job.reduce (k,i)))
                  >>| (function
                      | Core.Std.Result.Error _ ->
                          Response.send w (Response.JobFailed "Job failed")
                      | Core.Std.Result.Ok result ->
                          Response.send w (Response.ReduceResult result))
                )
                >>= (fun _ -> handle ())
        ) in
    handle ()
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


