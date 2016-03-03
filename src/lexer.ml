# 1 "src/lexer.mll"
 

exception Error of string

let error_msg lexbuf = 
  Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)
  |> fun s -> Error s
  |> raise


(* Terminal symbols for PL/O. *)
type token = 
  | Period      (* . *)
  | Const             (* const *)
  | Ident of string   (* x, y, z .. *)
  | Equal         (* = *)
  | Number of int     (* 123, 2, 0, etc *)
  | Comma             (* , *)
  | Semicolon         (* ; *)
  | Var           (* var *)
  | Assignment        (* := *)
  | Call              (* call *)
  | Read     (* ?  *)
  | Write    (* !  *) 
  | Begin         (* begin *)
  | End           (* end *)
  | If          (* if  *)
  | Then          (* then *)
  | While         (* while *)
  | Do          (* do *)
  | Odd           (* odd *)  
  | NotEqual        (* #  *)
  | LessThan        (* <  *)
  | LessThanEql       (* <= *)
  | GreaterThan       (* >  *)
  | GreaterThanEql    (* >= *)
  | Plus          (* +  *)
  | Minus         (* -  *)
  | Times             (* *  *)      
  | Divide        (* /  *)
  | Lparen            (* (  *)
  | Rparen            (* )  *)
  | Eof               (* end of file, '$' *)

module KeywordTbl =
  Map.Make(struct
    type t = string
    let compare a b =
      String.(compare (lowercase a) (lowercase b))
  end)
  
let keyword_tbl = 
  List.fold_left
  (fun tbl (kwd, tok) -> KeywordTbl.add kwd tok tbl)
  KeywordTbl.empty
  ["const", Const;   
   "var", Var;   
   "call", Call;   
   "begin", Begin;
   "end", End;
   "if", If;
   "then", Then;
   "while", While;
   "do", Do;
   "odd", Odd]

# 69 "src/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\234\255\235\255\075\000\150\000\238\255\239\255\240\255\
    \241\255\242\255\243\255\003\000\030\000\248\255\249\255\250\255\
    \031\000\252\255\253\255\254\255\002\000\251\255\247\255\245\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\019\000\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\011\000\009\000\255\255\255\255\255\255\
    \021\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\020\000\020\000\020\000\020\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\014\000\020\000\013\000\000\000\000\000\000\000\000\000\
    \006\000\005\000\008\000\010\000\017\000\009\000\019\000\007\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\016\000\000\000\012\000\018\000\011\000\015\000\
    \023\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\022\000\021\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\020\000\020\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\020\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\012\000\016\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec next_token lexbuf =
    __ocaml_lex_next_token_rec lexbuf 0
and __ocaml_lex_next_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 73 "src/lexer.mll"
            ( next_token lexbuf )
# 211 "src/lexer.ml"

  | 1 ->
# 74 "src/lexer.mll"
            ( Period )
# 216 "src/lexer.ml"

  | 2 ->
# 75 "src/lexer.mll"
            ( Equal )
# 221 "src/lexer.ml"

  | 3 ->
# 76 "src/lexer.mll"
            ( Comma )
# 226 "src/lexer.ml"

  | 4 ->
# 77 "src/lexer.mll"
            ( Assignment )
# 231 "src/lexer.ml"

  | 5 ->
# 78 "src/lexer.mll"
            ( Read )
# 236 "src/lexer.ml"

  | 6 ->
# 79 "src/lexer.mll"
            ( Write )
# 241 "src/lexer.ml"

  | 7 ->
# 80 "src/lexer.mll"
            ( NotEqual )
# 246 "src/lexer.ml"

  | 8 ->
# 81 "src/lexer.mll"
            ( LessThanEql )
# 251 "src/lexer.ml"

  | 9 ->
# 82 "src/lexer.mll"
            ( LessThan )
# 256 "src/lexer.ml"

  | 10 ->
# 83 "src/lexer.mll"
            ( GreaterThanEql )
# 261 "src/lexer.ml"

  | 11 ->
# 84 "src/lexer.mll"
            ( GreaterThan )
# 266 "src/lexer.ml"

  | 12 ->
# 85 "src/lexer.mll"
            ( Plus )
# 271 "src/lexer.ml"

  | 13 ->
# 86 "src/lexer.mll"
            ( Minus )
# 276 "src/lexer.ml"

  | 14 ->
# 87 "src/lexer.mll"
            ( Times )
# 281 "src/lexer.ml"

  | 15 ->
# 88 "src/lexer.mll"
            ( Divide )
# 286 "src/lexer.ml"

  | 16 ->
# 89 "src/lexer.mll"
            ( Lparen )
# 291 "src/lexer.ml"

  | 17 ->
# 90 "src/lexer.mll"
            ( Rparen )
# 296 "src/lexer.ml"

  | 18 ->
let
# 91 "src/lexer.mll"
         i
# 302 "src/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 91 "src/lexer.mll"
            ( Number (int_of_string i) )
# 306 "src/lexer.ml"

  | 19 ->
let
# 92 "src/lexer.mll"
         s
# 312 "src/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 92 "src/lexer.mll"
            ( try KeywordTbl.find s keyword_tbl
              with Not_found -> Ident (s) )
# 317 "src/lexer.ml"

  | 20 ->
# 94 "src/lexer.mll"
            ( Eof )
# 322 "src/lexer.ml"

  | 21 ->
# 95 "src/lexer.mll"
            ( error_msg lexbuf )
# 327 "src/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_next_token_rec lexbuf __ocaml_lex_state

;;

