open Async.Std

let get_app name = match MapReduce.get_app name with
  | None     -> failwith ("App " ^ name ^ " not installed")
  | Some app -> app

let run_local name args =
  let module App = (val (get_app name)) in
  let module AppMain = App.Make(LocalController.Make) in

  AppMain.main args >>| fun () -> (shutdown 0)

let run_remote name args addresses =
  let split_host_and_port s =
    match Str.split (Str.regexp_string ":") s with
      | [host; port] -> (host, int_of_string port)
      | _            -> failwith "invalid host:port"
  in

  let module App     = (val (get_app name)) in
  let module AppMain = App.Make(RemoteController.Make) in

  Reader.file_lines addresses >>= fun addresses ->

  print_endline "addresses:";
  List.iter print_endline addresses;
  let hps = List.map split_host_and_port addresses in
  RemoteController.init hps;
  AppMain.main args >>| fun () -> (shutdown 0)

let () =
  Command.async_basic
    ~summary:"Run the MapReduce controller"
    ~readme:AppList.list_apps
    Command.Spec.(
      empty
      +> flag "-local" no_arg
         ~doc:" run the app locally"
      +> flag "-addresses" (optional_with_default "addresses.txt" string)
         ~doc:"filename the file with the worker addresses"
      +> anon ("app" %: string)
      +> anon (sequence ("arg" %: string))
    )
    (fun local addresses app args () ->
      if local then run_local  app args
               else run_remote app args addresses
    )
  |> Command.run

