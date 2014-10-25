(* Functions for dealing with environments. *)

(* Types *)
type 'a binding = Identifier.variable * 'a
type 'a environment = 'a binding list

(* Constants *)
let empty_environment = []

(* Binding related functions *)
let add_binding env bnd = bnd :: env
let is_bound env name = List.mem_assoc name env
let get_binding env name = List.assoc name env

let combine_environments env env' = env @ env'

(* Stringifing functions *)
let string_of_binding string_of_value (var, value) =
  (Identifier.string_of_variable var) ^ " -> " ^ (string_of_value value)

let string_of_environment string_of_value env =
  let string_of_binding = string_of_binding string_of_value in
  let stringified_bindings = List.map string_of_binding env in
  let all_bindings_string = String.concat ", " stringified_bindings in
  all_bindings_string
