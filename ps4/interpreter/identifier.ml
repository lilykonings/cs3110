(* Functions for dealing with identifiers. *)

(* identifier has the representation invariant that it contains no uppercase
   characters.  *)
type identifier = string

let identifier_of_string str = String.lowercase str

let string_of_identifier id = id

let keywords =
  ["quote"; "if"; "lambda"; "define"; "set!"; "let"; "let*"; "letrec"]

let is_keyword id = List.mem id keywords
let is_valid_variable id = not (is_keyword id)

(* variable has the representation invariant that it is not one of the keywords
   listed above. *)
type variable = identifier

let variable_of_identifier id =
  let () = assert (is_valid_variable id) in id

let string_of_variable var = var
