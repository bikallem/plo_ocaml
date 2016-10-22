open Lexer
open Ast

type parse_buffer

val default_pb : string -> parse_buffer                                                                               
val next : parse_buffer -> parse_buffer 
val expect_error : parse_buffer -> token -> string -> 'a 
val error : parse_buffer -> 'a -> 'b 
val is_same : token -> token -> bool 
val is_token_in : token list -> token -> bool 
val expect : token -> parse_buffer -> parse_buffer * bool 
val parse_factor : parse_buffer -> parse_buffer * factor 
val parse_term : parse_buffer -> parse_buffer * term 
val parse_expression : parse_buffer -> parse_buffer * expression 
val parse_condition : parse_buffer -> parse_buffer * condition 
val identifier : string -> parse_buffer -> parse_buffer * string 
val parse_statement : parse_buffer -> parse_buffer * statement 
val number : string -> parse_buffer -> parse_buffer * int 
val parse_block : parse_buffer -> parse_buffer * block 
val program : parse_buffer -> program 
val parse_plo : Lexing.lexbuf -> program 
