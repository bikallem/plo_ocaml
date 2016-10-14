exception Syntax_error of string
type parse_buffer = { lookahead : Lexer.token; lexbuf : Lexing.lexbuf; }
val parse_factor : parse_buffer -> parse_buffer * Ast.factor
val parse_term : parse_buffer -> parse_buffer * Ast.term
val parse_expression : parse_buffer -> parse_buffer * Ast.expression
val parse_condition : parse_buffer -> parse_buffer * Ast.condition
val identifier : string -> parse_buffer -> parse_buffer * string
val parse_statement : parse_buffer -> parse_buffer * Ast.statement
val number : string -> parse_buffer -> parse_buffer * int
val parse_block : parse_buffer -> parse_buffer * Ast.block
val program : parse_buffer -> Ast.program
val parse_plo : Lexing.lexbuf -> Ast.program
