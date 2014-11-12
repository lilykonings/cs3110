open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

(* Convert nested cons into a list *)
(* Requires: datum should not be Atom and values stored in cons should all be same type *)
(* Returns: a list of values in cons or an error if inputted an Atom *)
let rec list_of_cons (d : datum) : 'a list =
  match d with
  | Atom _ -> failwith "Error: requires cons"
  | Cons (h,t) -> h::(list_of_cons t)
  | Nil -> []

let variable_of_datum (d : datum) : variable =
  match d with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
    Identifier.variable_of_identifier id
  | _ -> failwith "Error: datum cannot be converted to variable"

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id -> ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) -> failwith "Error: usage of keyword without arguments"
  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b)
  | Atom (Integer n) -> ExprSelfEvaluating (SEInteger n)
  | Cons (d, Nil) -> read_expression d
  | Cons (Atom (Identifier id), d) when Identifier.is_valid_variable id ->
    ExprProcCall (ExprVariable (Identifier.variable_of_identifier id), List.map read_expression (list_of_cons d))
  | Cons (Atom (Identifier id), datum2) ->
    begin
      match Identifier.string_of_identifier id, datum2 with
      | "quote", d -> ExprQuote d
      | "if", Cons (d1,Cons (d2,d3)) ->
        ExprIf (read_expression d1, read_expression d2, read_expression d3)
      | "if", _ -> failwith "Error: invalid arguments for if keyword"
      | "lambda", Cons (d1,d2) ->
        ExprLambda (List.map variable_of_datum (list_of_cons d1), List.map read_expression (list_of_cons d2))
      | "lambda", _ -> failwith "Error: invalid arguments for lambda keyword"
      | "set!", Cons (d1,d2) -> ExprAssignment (variable_of_datum d1, read_expression d2)
      | "set!", _ -> failwith "Error: invalid arguments for set! keyword"
      | "let", Cons (d1,d2) ->
        ExprLet (List.map (
          fun x ->
            match x with
            | Cons (a,b) -> (variable_of_datum a,read_expression b)
            | _ -> failwith "Error: invalid arguments for let keyword"
        ) (list_of_cons d1), List.map read_expression (list_of_cons d2))
      | "let", _ -> failwith "Error: invalid arguments for let keyword"
      | "let*", Cons (d1,d2) ->
        ExprLet (List.map (
          fun x ->
            match x with
            | Cons (a,b) -> (variable_of_datum a,read_expression b)
            | _ -> failwith "Error: invalid arguments for let* keyword"
        ) (list_of_cons d1), List.map read_expression (list_of_cons d2))
      | "let*", _ -> failwith "Error: invalid arguments for let* keyword"
      | "letrec", Cons (d1,d2) ->
        ExprLet (List.map (
          fun x ->
            match x with
            | Cons (a,b) -> (variable_of_datum a,read_expression b)
            | _ -> failwith "Error: invalid arguments for letrec keyword"
        ) (list_of_cons d1), List.map read_expression (list_of_cons d2))
      | "letrec", _ -> failwith "Error: invalid arguments for letrec keyword"
      | _,_ -> failwith "Error: invalid keyword"
    end
  | Cons (_,_) -> failwith "Error: invalid cons arguments"
  | _ -> failwith "Error: datum does not match expression grammar"

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match input with
  | Cons (Atom (Identifier i), datum2) when Identifier.string_of_identifier i = "define" ->
  begin
    match datum2 with
    | Cons (Atom (Identifier id), exp) when Identifier.is_valid_variable id -> 
      ToplevelDefinition ((Identifier.variable_of_identifier id), (read_expression exp)) 
    | _ -> failwith "Not valid identifier" 
  end
  | _ -> ToplevelExpression (read_expression input)

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
let rec eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating (SEBoolean b) -> ValDatum (Atom (Boolean b))
  | ExprSelfEvaluating (SEInteger n) -> ValDatum (Atom (Integer n))
  | ExprVariable var ->
    begin
      try !(Environment.get_binding env var)
      with Not_found -> failwith "Error: no binding for this variable in environment"
    end
  | ExprQuote datum -> ValDatum datum
  | ExprLambda (v,e) -> ValProcedure (ProcLambda (v, env, e))
  | ExprProcCall (ExprVariable h,t) ->
    (try
          let f = !(Environment.get_binding env h) in
          match f with
          | ValProcedure (ProcBuiltin x) -> x (List.map (fun x -> eval x env) t) env
          | ValProcedure (ProcLambda (v,envir,e)) ->
            let env2 =
              List.fold_left2 (fun acc a b ->
                Environment.add_binding acc (a,ref (eval b acc))
              ) envir v t in
            List.fold_left (fun acc x -> eval x env2) (ValDatum (Atom (Integer 0))) e
          | _ -> failwith "Error: not valid procedure call"
        with Not_found -> failwith "Error: no binding for this variable in environment")
  | ExprIf (b, e1, e2) ->
    (match eval b env with
        | ValDatum (Atom (Boolean b)) ->
          if b then eval e1 env
          else eval e2 env
        | _ -> eval e1 env)
  | ExprAssignment (var, exp) ->
    if not (Environment.is_bound env var) then
      failwith "Error: The variable is not bound"
    else
      let bnd = Environment.get_binding env var in
      bnd := (eval exp env); ValDatum (Nil)
  | _ -> failwith "unfinished"

(* This function returns an initial environment with any built-in
   bound variables. *)
let initial_environment () : environment =
  let env1 = Environment.empty_environment in
  let env2 =
    Environment.add_binding env1
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "course"),
        ref (ValDatum (Atom (Integer 3110)))) in
  let env3 =
    Environment.add_binding env2
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "car"),
        ref (ValProcedure (ProcBuiltin (
        fun l env -> match l with
        | h1::h2::[] -> h1
        | _ -> failwith "Error: not valid elements in list for car"
      )))) in
  let env4 =
    Environment.add_binding env3
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "cdr"),
        ref (ValProcedure (ProcBuiltin (
        fun l env -> match l with
        | h1::h2::[] -> h2
        | _ -> failwith "Error: not valid elements in list for cdr"
      )))) in
  let env5 =
    Environment.add_binding env4
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "cons"),
        ref (ValProcedure (ProcBuiltin (
        fun l env -> match l with
        | (ValDatum h1)::(ValDatum h2)::[] -> 
            ValDatum (Cons (h1, h2))
        | _ -> failwith "Error: not valid elements in list for cons"
      )))) in
  let env6 =
    Environment.add_binding env5
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "+"),
        ref (ValProcedure (ProcBuiltin (
        fun l env -> ValDatum (Atom (Integer (List.fold_left (
          fun acc x -> match x with
          | ValDatum (Atom (Integer n)) -> acc + n
          | ValDatum (Atom (Identifier id)) when Identifier.is_valid_variable id ->
            begin
              match !(Environment.get_binding env (Identifier.variable_of_identifier id)) with
              | ValDatum (Atom (Integer n)) -> acc + n
              | _ -> failwith "Error: not valid elements in list for add"
            end
          | _ -> failwith "Error: not valid elements in list for add"
        ) 0 l)))
      )))) in
  let env7 =
    Environment.add_binding env6
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "*"),
        ref (ValProcedure (ProcBuiltin (
        fun l env -> ValDatum (Atom (Integer (List.fold_left (
          fun acc x -> match x with
          | ValDatum (Atom (Integer n)) -> acc * n
          | ValDatum (Atom (Identifier id)) when Identifier.is_valid_variable id ->
            begin
              match !(Environment.get_binding env (Identifier.variable_of_identifier id)) with
              | ValDatum (Atom (Integer n)) -> acc * n
              | _ -> failwith "Error: not valid elements in list for mult"
            end
          | _ -> failwith "Error: not valid elements in list for mult"
        ) 1 l)))
      )))) in
  let env8 =
    Environment.add_binding env7
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "equal?"),
        ref (ValProcedure (ProcBuiltin (
        fun l env -> match l with
        | h1::h2::[] -> ValDatum (Atom (Boolean (h1 = h2)))
        | _ -> failwith "Error: not valid elements in list for equal?"
      )))) in
  let env9 =
    Environment.add_binding env8
      (Identifier.variable_of_identifier (Identifier.identifier_of_string "eval"),
        ref (ValProcedure (ProcBuiltin (
        fun l env -> match l with
        | (ValDatum h)::t -> eval (read_expression h) env
        | _ -> failwith "Error: not valid elements in list for eval"
      )))) in
  env9

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (var, exp) ->
    if (Environment.is_bound env var) = false then
      ((eval exp (Environment.add_binding env (var, ref (ValDatum (Nil))))), 
        (Environment.add_binding env (var, ref (eval exp (Environment.add_binding env (var, ref (ValDatum (Nil))))))))
    else
      ((eval exp env), Environment.add_binding env (var, ref (eval exp env)))

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"