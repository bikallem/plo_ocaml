type expression_op =
  | Plus
  | Minus
  [@@deriving show]
  
type logical_op =
  | Equal
  | NotEqual
  | LessThan
  | LessThanEql
  | GreaterThan
  | GreaterThanEql
  [@@deriving show]

type term_op =
  | Multiply
  | Divide
    [@@deriving show]

type factor =
  | Identifier of string
  | Number of int
  | Expr of expression
  [@@deriving show]
and term = factor * (term_op * factor) list [@@deriving show]
and expression = start_expression * (expression_op * term) list [@@deriving show]
and start_expression = expression_op option * term [@@deriving show]

type condition =
  | Odd of expression
  | Logical of expression * logical_op * expression
  [@@deriving show]               

type identifier = string [@@deriving show]
type number = int [@@deriving show]

type statement =
  | Assignment of identifier * expression
  | Call of identifier
  | Read of identifier
  | Write of identifier
  | BeginEnd of statement * statement list
  | IfThen of condition * statement
  | WhileDo of condition * statement
  | Empty
  [@@deriving show]

type constant = (identifier * number) [@@deriving show]
type var = identifier [@@deriving show]

type block =
  | Block of constant list * var list * procedure list * statement
  [@@deriving show]
and procedure = identifier * block [@@deriving show]
                                               

