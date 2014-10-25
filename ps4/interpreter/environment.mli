(** The [Environment] module provides functions for dealing with variable
    binding in environments.

    In the environment model of interpretation, we maintain a map from variable
    names that we know about to the values associated with them.  Each
    variable-value association is called a {e binding}, and we may represent a
    binding as a tuple between an variable and a value.  In order to make the
    [Environment] module as general as possible, we let the value be a type
    parameter.  That way, users of this module can store some value type in
    their bindings which is some sort of algebraic data type useful for whatever
    interpreter they are writing; some value type that is actually a ref-cell,
    for supporting mutable state; or some more complex locator, which allows
    variables to be bound to values stored in a different data structure.

    An [Environment.environment] is a collection of bindings that are associated
    with the current lexical scope in the Scheme program.  For example, a [(let
    bindings body)] creates a new lexical scope that has the bindings given in
    which to evaluate body; thus body has its own environment.  At any given
    scope, the variable bindings in parent scopes should still be valid.  When
    searching for variable bindings, we look on in a LIFO (last-in-first-out)
    way, so that bindings that are more recent (i.e., in the most recent scope)
    are found before bindings in less recent scopes.  This captures and
    formalizes the notion that local variables should be found instead of global
    variables with the same name, and allows us to define unbound variables as
    variables that do not appear in our environment.

    All data structures in this module are functional, so no operations in this
    module mutate their inputs. *)

(** A mapping between an variable name and the value associated with that
    variable in the current scope. *)
type 'a binding = Identifier.variable * 'a

(** An ordered list of [Environment.binding]s that represent the total set 
    bindings available for use in evaluation at some point. *)
type 'a environment

(** An environment that contains no bindings.  All environments must be
    constructed by operating on this environment. *)
val empty_environment: 'a environment

(** Returns an environment that corresponds to the given existing environment
    with a new binding added so that it will be found before any less recent
    bindings of the same variable.  The original environment is not modified.

    {b Postconditions}:
    For the original environment [env], the added binding [(var, value)], and
    the new environment [env' <- add_binding env (var, value)],
    - [is_bound env' var]
    - [(get_binding env' var) = val]
    - For all variables [var'], [var' <> var] implies
      [(is_bound env var') = (is_bound env' var')]
    - For all variables [var'], [var' <> var] and [is_bound env var'] imply
      [(get_binding env var') = (get_binding env' var')]. *)
val add_binding: 'a environment -> 'a binding -> 'a environment

(** Returns whether a given variable is bound in the given environement.  The
    original environment is not modified. *)
val is_bound: 'a environment -> Identifier.variable -> bool

(** Returns the value associated with the given variable in the given
    environment.  The original environment is not modified.

    {b Preconditions}:
    For the given environment [env] and given variable [var],
    - [is_bound env var] *)
val get_binding: 'a environment -> Identifier.variable -> 'a

(** Returns an environment that corresponds to combining the two argument
    environments together.  The bindings in the first environment take
    precedence over the bindings in the second environment.  In other words, the
    first environment is the more local scope of the two environments.  Neither
    of the original environments are modified.

    {b Postconditions}:
    For the first original environment [env], the second original environment
    [env'], and new environment [result <- combine_environments env env'],
    - For any variable [var], [is_bound env var] implies [is_bound result var]
    - For any variable [var], [is_bound env' var] implies [is_bound result var]
    - For any variable [var], [is_bound result var] implies [is_bound env var]
      or [is_bound env' var].
    - For any variable [var], [is_bound env var] implies
      [(get_binding result var) = (get_binding env var)]
    - For any variable [var], [is_bound env' var] and [not (is_bound env var)]
      implies [(get_binding result var) = (get_binding env' var)]
*)
val combine_environments: 'a environment -> 'a environment -> 'a environment

(** Returns a string that represents an [Environment.binding].  Because the type
    parameter to a [binding] is unknown, we also take in a function that
    converts the bound value to a string. *)
val string_of_binding: ('a -> string) -> 'a binding -> string

(** Returns a string that represents an [Environment.environment], including all
    its bindings, with the most local bindings being printed first.  Because the
    type parameter to an [environment] is unknown, we also take in a function
    that converts the bound values to strings. *)
val string_of_environment: ('a -> string) -> 'a environment -> string
