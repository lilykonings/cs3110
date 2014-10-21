(** The [Ast] module provides types and functions that represent a parsed and
    desugared syntax tree for Scheme. *)

(** The name of an identifier.  This is simply a synonym for
    [Identifier.identifier]. *)
type identifier = Identifier.identifier
(** The name of a variable.  This is simply a synonym for
    [Identifier.variable]. *)
type variable = Identifier.variable

(* EXTERNAL REPRESENTATION GRAMMAR: *)

(** An atom *)
type atom =
  (** Boolean data may be either the true value [#t], or the false value [#f].
      We store it directly as an OCaml boolean. *)
  | Boolean of bool
  (** Integer data may be any integer that is representable by the [int] type in
      OCaml. *)
  | Integer of int
  (** An identifier datum may be any valid identifier, including keywords. *)
  | Identifier of identifier

(** A datum, which forms the basis of our external representation.  Each datum
    may be an atom, a cons cell, or the empty list (nil). *)
type datum =
  (** An atom may be either a boolean, integer, or identifier. *)
  | Atom of atom
  (** A cons-cell, written as [(a . b)], is composed of two cells: the car and
      the cdr.  Both the car and the cdr can be arbitrary data. *)
  | Cons of datum * datum
  (** The empty list [()] is also called nil. *)
  | Nil

(* SCHEME3110 EXPRESSION SYNTAX *)

(** A self-evaluating expression is an expression that evaluates to itself (or
    more precisely, to a datum value with the same representation). *)
type self_evaluating =
  (** Boolean self-evaluating expressions may be either [#t] or [#f], and are
      stored as an OCaml boolean. *)
  | SEBoolean of bool
  (** Integer self-evaluating expressions may be any integer representable by an
      OCaml integer. *)
  | SEInteger of int

(** A let binding will eventually produce a binding in the environment.  It
    consists of a variable to bind and an expression to evaluate to bind. *)
and let_binding = variable * expression

(** An expression is something that can be evaluated. *)
and expression =
  (** A self-evaluating expression evaluates to a value with the same
      representation. *)
  | ExprSelfEvaluating of self_evaluating
  (** A variable which should be looked up in the current environment. *)
  | ExprVariable of variable
  (** A quoted datum evaluates to the datum it contains. *)
  | ExprQuote of datum
  (** A lambda expression evaluates to a procedure with the given variables as
      arguments and the given expressions as the body.  Both lists must not be
      empty.  *)
  | ExprLambda of variable list * expression list
  (** A procedure call expression calls the given procedure with the the result
      of evaluating the given argument expressions. *)
  | ExprProcCall of expression * expression list
  (** A conditional if expression takes in a condition expression and both an
      expression to evaluate when the condition is true and an expression to
      evaluate when the condition is false. *)
  | ExprIf of expression * expression * expression
  (** An assignment (set!) mutates the binding of a variable to the result of
      evaluating a given expression. *)
  | ExprAssignment of variable * expression
  (** A let expression binds all its let bindings simultaneously and evaluates
      the body list, which may not be empty.  See the writeup for details. *)
  | ExprLet of let_binding list * expression list
  (** A let* expression binds all its let bindings in sequence and evaluates the
      body list, which may not be empty.  See the writeup for details. *)
  | ExprLetStar of let_binding list * expression list
  (** A letrec expression binds all its variables to a dummy value, evaluates
      all the let binding expressions, and then mutates all the variables to the
      result of these expressions.  It then evaluates the body list, which may
      not be empty.  See the writeup for details. *)
  | ExprLetRec of let_binding list * expression list

(** Represents toplevel input.  We use this type to make sure that definitions
    only occur at the toplevel. *)
type toplevel =
  (** Any expression can occur at the toplevel. *)
  | ToplevelExpression of expression
  (** A definition may only occur at the toplevel.  It binds the variable to the
      result of evaluating the expression. *)
  | ToplevelDefinition of variable * expression
