(** The [Eval] module transforms unevaluated expression trees to evaluated
    expression trees. *)

(** A built-in procedure is implemented as an OCaml function with this type.  It
    takes in a list of values as arguments and returns a single value. *)
type builtin = value list -> environment -> value
(** A procedure may be either a builtin or a lambda procedure defined by the
    user.  These can be called by procedure application. *)
and procedure =
  (** We store a builtin simply as the OCaml function that implements it. *)
  | ProcBuiltin of builtin
  (** A lambda procedure is stored as a non-empty list of arguments, a closure
      environment, and a non-empty list of body expressions to evaluate on
      procedure application. *)
  | ProcLambda of Ast.variable list * environment * Ast.expression list

(** A value is the result of evaluating something at the toplevel. *)
and value =
  (** An arbitrary datum may be the result of evaluation. *)
  | ValDatum of Ast.datum
  (** A procedure may also be the result of evaluation.  This cannot be written
      as a datum (you can't write a closure or a builtin as a datum; they are
      instead the result of evaluation of some expression). *)
  | ValProcedure of procedure

(** A binding is a map between a variable and a ref-cell containing a value. *)
and binding = value ref Environment.binding
(** An environment is a set of bindings. *)
and environment = value ref Environment.environment

(** The environment in which the interpreter should start.  This should contain
    all builtins you want for your session.  This is a function so you can
    safely raise exceptions within it, and the interpreter will still work
    (albeit with an empty environment). *)
val initial_environment: unit -> environment

(** A function that takes in a datum and returns a toplevel input that this
    datum corresponds to.  If no toplevel input can be formed from the given
    datum, raise an exception. *)
val read_toplevel: Ast.datum -> Ast.toplevel

(** A function that takes in a datum and returns an expression that this datum
    corresponds to.  If no expression can be formed from the given datum (even
    if it is a valid toplevel input), raise an exception. *)
val read_expression: Ast.datum -> Ast.expression

(** A function that takes in a toplevel input and an environment in which to
    evaluate the toplevel input, and returns a value that results from
    evaluation with an updated environment. *)
val eval_toplevel: Ast.toplevel -> environment ->
		   value * environment

(** A function that takes an expression and an environment in which to evaluate
    that expression, and returns a value that results from evaluation. *)
val eval: Ast.expression -> environment -> value

(** Returns a string that corresponds to a value. *)
val string_of_value: value -> string

