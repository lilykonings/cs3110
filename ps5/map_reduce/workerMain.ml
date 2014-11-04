open Async.Std

let () = Command.async_basic
    ~summary:"Run a MapReduce worker"
    ~readme:AppList.list_apps
    Command.Spec.(
      empty
      +> anon ("port" %: int)
    )
    (fun port () -> Worker.init port)
  |> Command.run

