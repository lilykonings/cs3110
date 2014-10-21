(** The [Identifier] module provides functions for dealing with identifier names
    in Scheme. *)

(** An identifier name. *)
type identifier

(** A variable name. *)
type variable

(** Constructs a new identifier from a string.

    {b Preconditions}:
    The string argument is a valid Scheme identifier name.

    {b Postconditions}:
    The identifier returned will compare equal to any equivalent Scheme
    identifiers. *)
val identifier_of_string: string -> identifier

(** Constructs a new variable from an identifier.

    {b Preconditions}:
    [is_valid_variable id]

    {b Postconditions}:
    The variable returned will compare equal to any equivalent Scheme
    identifier. *)
val variable_of_identifier: identifier -> variable

(** Returns a string that represents an [Identifier.identifier]. *)
val string_of_identifier: identifier -> string

(** Returns a string that represents an [Identifier.variable]. *)
val string_of_variable: variable -> string

(** Returns whether an identifier is a Scheme3110 keyword. *)
val is_keyword: identifier -> bool

(** Returns whether an identifier is a valid variable name (that is, it is not a
    keyword. *)
val is_valid_variable: identifier -> bool
