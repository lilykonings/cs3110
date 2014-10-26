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

(* This function returns an initial environment with any built-in
   bound variables. *)
let initial_environment () : environment =
  [("course",3110);
  ("car",fun (x,y) -> x);
  ("cdr",fun (x,y) -> y);
  ("cons",fun x y -> (x,y));
  ("+",);
  ("-",);
  ("equal?",fun x y -> x = y);
  ("eval",fun x y -> eval x y)]

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
let eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating (SEBoolean b) -> ValDatum (Atom (Boolean b))
  | ExprSelfEvaluating (SEInteger n) -> ValDatum (Atom (Integer n))
  | ExprVariable var ->
    match find var env with
    | Some val -> ValDatum () (* FIX THIS LATER *)
    | None -> failwith "Error: no binding found for " ^ var
  | ExprQuote datum -> 
  | ExprLambda (_, _) -> 
  | ExprProcCall _ ->
  | ExprIf (b, e1, e2) ->
    match eval b env with
    | ExprSelfEvaluating (SEBoolean b) ->
      if b then eval e1 env
      else eval e2 env
    | _ -> failwith "Error: if statement does not have valid bool exp"
  | ExprAssignment (_, _) ->
  | ExprLet (_, _) ->
  | ExprLetStar (_, _) ->
  | ExprLetRec (_, _) ->

(* Finds value of variable in env if exists, otherwise return None *)
(* let find (var : variable) (env : environment) : value option =
  try Some (List.assoc var env)
  with Not_found -> None *)

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

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
