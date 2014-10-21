type identifier        = Identifier.identifier
type variable          = Identifier.variable

type atom =
  | Boolean            of bool
  | Integer            of int
  | Identifier         of identifier

type datum =
  | Atom               of atom
  | Cons               of datum * datum
  | Nil

type self_evaluating =
  | SEBoolean          of bool
  | SEInteger          of int

and let_binding        = variable * expression

and expression =
  | ExprSelfEvaluating of self_evaluating
  | ExprVariable       of variable
  | ExprQuote          of datum
  | ExprLambda         of variable list * expression list
  | ExprProcCall       of expression * expression list
  | ExprIf             of expression * expression * expression
  | ExprAssignment     of variable * expression
  | ExprLet            of let_binding list * expression list
  | ExprLetStar        of let_binding list * expression list
  | ExprLetRec         of let_binding list * expression list

type toplevel =
  | ToplevelExpression of expression
  | ToplevelDefinition of variable * expression
