let parse (s : string) : Ast.datum list =
  Parser.main Lexer.token (Lexing.from_string s)

let print_bindings env =
  let printer expr = Eval.string_of_value !expr in
  print_endline (Environment.string_of_environment printer env)

let print_startup () =
let startup = [
      "****************************************************************";
      "Welcome to Scheme3110.";
      "  Type ^D (CTRL+D) to exit";
      "  Type #bindings to print the current variable bindings";
      "  Type #restart to start with a fresh environment";
      "  Input your Scheme3110 code, and press enter twice to evaluate.";
      "****************************************************************"
    ] in
  begin
    List.iter print_endline startup;
    print_newline ()
  end

let print_eval value =
  print_endline (Eval.string_of_value value)

let do_eval env datum =
  let toplevel_input = Eval.read_toplevel datum in
  let (value, env) = Eval.eval_toplevel toplevel_input env in
  begin
    print_eval value;
    env
  end

(* read lines from the console, appending them to s, until the user
   enters a blank line *)
let read_console () =
  let rec read_lines s =
    let input = read_line () in
    if input = "" then s
    else read_lines (s ^ input ^ "\n") in
  read_lines ""

let create_initial_environment () =
  try
    Eval.initial_environment ()
  with
  | exn ->
     begin
       print_endline ("Exception raised when trying to create initial "
		      ^ "environment (did you implement\n"
		      ^ "Eval.initial_environment?)");
       print_endline ("  " ^ (Printexc.to_string exn));
       print_endline "Using empty environment instead...";
       Environment.empty_environment
     end

let rec do_repl env =
  let prompt = "zardoz> " in

  try
    print_string prompt;
    let input = read_console () in
    if input = "#bindings\n" then
      begin
        print_bindings env;
        do_repl env
      end
    else if input = "#restart\n" then
      do_repl (create_initial_environment ())
    else
      let data = parse input in
      let env = List.fold_left do_eval env data in
      do_repl env
  with
  | End_of_file -> print_endline ""
  | exn ->
     begin
       print_endline (Printexc.to_string exn);
       do_repl env
     end

let repl () =
  print_startup ();
  do_repl (create_initial_environment ())

let _ = repl ()
