(** Lexer.
  A [Lexer] recognizes terminal symbols for PL/0. *)
{  
exception Lexer_error of string
(** Raised when [!Lexer] encounters unrecongnized character. *)

let error_msg lexbuf = 
  Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)
  |> fun s -> Lexer_error s
  |> raise

(** Represents terminal symbols for PL/O. *)
type token = 
  | Period      (** . *)
  | Const             (** const *)
  | Ident of string   (** x, y, z .. *)
  | Equal         (** = *)
  | Number of int     (** 123, 2, 0, etc *)
  | Comma             (** , *)
  | Semicolon         (** ; *)
  | Var           (** var *)
  | Procedure      (** procedure *)
  | Assignment        (** := *)
  | Call              (** call *)
  | Read     (** ?  *)
  | Write    (** !  *) 
  | Begin         (** begin *)
  | End           (** end *)
  | If          (** if  *)
  | Then          (** then *)
  | While         (** while *)
  | Do          (** do *)
  | Odd           (** odd *)  
  | NotEqual        (** #  *)
  | LessThan        (** <  *)
  | LessThanEql       (** <= *)
  | GreaterThan       (** >  *)
  | GreaterThanEql    (** >= *)
  | Plus          (** +  *)
  | Minus         (** -  *)
  | Times             (* *  *)      
  | Divide        (** /  *)
  | Lparen            (** (  *)
  | Rparen            (** )  *)
  | Eof               (** end of file also known as ['$'] *)  

let to_string t = 
  match t with 
  | Period            -> "."
  | Const             -> "const"
  | Ident s           -> s
  | Equal             -> "="
  | Number i          -> string_of_int i
  | Comma             -> ","
  | Semicolon         -> ";"
  | Var               -> "var"
  | Procedure         -> "procedure"
  | Assignment        -> ":="
  | Call              -> "call"
  | Read              -> "?"
  | Write             -> "!" 
  | Begin             -> "begin"
  | End               -> "end"
  | If                -> "if"
  | Then              -> "then"
  | While             -> "while"
  | Do                -> "do"
  | Odd               -> "odd"  
  | NotEqual          -> "#"
  | LessThan          -> "<"
  | LessThanEql       -> "<="
  | GreaterThan       -> ">"
  | GreaterThanEql    -> ">="
  | Plus              -> "+"
  | Minus             -> "-"
  | Times             -> "*"      
  | Divide            -> "/"
  | Lparen            -> "("
  | Rparen            -> ")"
  
module KeywordTbl =
  Map.Make(struct
    type t = string
    let compare a b =
      String.(compare (lowercase_ascii a) (lowercase_ascii b))
  end)
  
let keyword_tbl = 
  List.fold_left
  (fun tbl (kwd, tok) -> KeywordTbl.add kwd tok tbl)
  KeywordTbl.empty
  ["const", Const;   
   "procedure", Procedure;
   "var", Var;   
   "call", Call;   
   "begin", Begin;
   "end", End;
   "if", If;
   "then", Then;
   "while", While;
   "do", Do;
   "odd", Odd]
}

let str = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']*
let ws = [' ' '\t' '\n']+
let num = ['0'-'9']+

rule next_token = parse 
|  ws       { next_token lexbuf } (* skip blanks. *)
| '.'       { Period }
| '='       { Equal }
| ','       { Comma }
| ":="      { Assignment }
| '?'       { Read }
| '!'       { Write }
| '#'       { NotEqual }
| "<="      { LessThanEql }
| '<'       { LessThan }
| ">="      { GreaterThanEql }
| '>'       { GreaterThan }
| '+'       { Plus }
| '-'       { Minus }
| '*'       { Times }
| '/'       { Divide }
| '('       { Lparen }
| ')'       { Rparen }
| num as i  { Number (int_of_string i) }
| str as s  { try KeywordTbl.find s keyword_tbl
              with Not_found -> Ident (s) }
| eof       { Eof }
| _         { error_msg lexbuf }
(** Retrieves the next recongnized token from [lexbuf]. *)
