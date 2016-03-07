type expression_op =
  | Plus
  | Minus

type condition_op =
  | Equal
  | NotEqual
  | LessThan
  | LessThanEql
  | GreaterThan
  | GreaterThanEql

type term_op =
  | Multiply
  | Divide

type factor =
  | Identifier of string
  | Number of int
  | Expr of expression
and term = factor * (term_op * factor) list
and expression = start_expression * (expression_op * term) list
and start_expression = expression_op option * term

type condition =
  | Odd of expression
  | Logical of expression * condition_op * expression

type identifier = string
type number = int

type statement =
  | Assignment of identifier * expression
  | Call of identifier
  | Read of identifier
  | Write of identifier
  | BeginEnd of statement * statement list
  | IfThen of condition * statement
  | WhileDo of condition * statement
  | Empty

type constant = (identifier * number)
type var = identifier

type block =
  | Block of constant list * var list * procedure list * statement
and procedure = identifier * block

