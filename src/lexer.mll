(** Lexer.
  A [Lexer] recognizes terminal symbols for PL/0. *)
{  
exception Lexer_error of string
(** Raised when [!Lexer] encounters unrecongnized character. *)

let error_msg c lexbuf = 
  let e = Printf.sprintf "Unrecognized character '%c' at offset %d: " c (Lexing.lexeme_start lexbuf) in 
  raise (Lexer_error e)  

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
  | Const             -> "CONST"
  | Ident s           -> s
  | Equal             -> "="
  | Number i          -> string_of_int i
  | Comma             -> ","
  | Semicolon         -> ";"
  | Var               -> "VAR"
  | Procedure         -> "PROCEDURE"
  | Assignment        -> ":="
  | Call              -> "CALL"
  | Read              -> "?"
  | Write             -> "!" 
  | Begin             -> "BEGIN"
  | End               -> "END"
  | If                -> "IF"
  | Then              -> "THEN"
  | While             -> "WHILE"
  | Do                -> "DO"
  | Odd               -> "ODD"  
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
  | Eof               -> ""
  
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
  ["CONST", Const;   
   "PROCEDURE", Procedure;
   "VAR", Var;   
   "CALL", Call;   
   "BEGIN", Begin;
   "END", End;
   "IF", If;
   "THEN", Then;
   "WHILE", While;
   "DO", Do;
   "ODD", Odd]
}

let str = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']*
let ws = [' ' '\t' '\n']+
let num = ['0'-'9']+

rule next_token = parse 
|  ws       { next_token lexbuf } (* skip blanks. *)
| '.'       { Period }
| ';'       { Semicolon }
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
| _   as c  { error_msg c lexbuf }

(** Retrieves the next recongnized token from [lexbuf]. *)
