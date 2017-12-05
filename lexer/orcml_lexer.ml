open Base
open Orcml.Parser

type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
              pos_fname = file;
              pos_lnum = 0;
              pos_bol = 0;
              pos_cnum = 0;
            }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
     pos_lnum = lcp.pos_lnum + 1;
     pos_bol = lcp.pos_cnum;
    }

let lexeme { stream } = Sedlexing.Utf8.lexeme stream

exception SyntaxError of string

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

let integer = [%sedlex.regexp? ('1'..'9', Star ('0'..'9')) | '0']

let digit = [%sedlex.regexp? '0'..'9']
let frac = [%sedlex.regexp? ('.', Star digit)]
let exp = [%sedlex.regexp? (('e' | 'E'), Opt ('-' | '+'), Plus digit)]

let float = [%sedlex.regexp? (Star digit, Opt frac, Opt exp)]

let newline = [%sedlex.regexp? ('\n' | '\r' | "\r\n")]

let id = [%sedlex.regexp? (('a'..'z' | 'A'..'Z' | '_'), Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9' | '\''))]

let rec token lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | Plus (Chars " \t\r") -> update lexbuf; token lexbuf
  | newline -> new_line lexbuf; token lexbuf
  | eof -> update lexbuf; EOF
  | "true" -> update lexbuf; TRUE
  | "false" -> update lexbuf; FALSE
  | "null" -> update lexbuf; NULL
  | "signal" -> update lexbuf; SIGNAL
  | "stop" -> update lexbuf; STOP
  | "type" -> update lexbuf; TYPE
  | "val" -> update lexbuf; VAL
  | "refer from" -> update lexbuf; REFER
  | "import" -> update lexbuf; IMPORT
  | "include" -> update lexbuf; INCLUDE
  | "lambda" -> update lexbuf; LAMBDA
  | "as" -> update lexbuf; AS
  | "def" -> update lexbuf; DEF
  | "if" -> update lexbuf; IF
  | "then" -> update lexbuf; THEN
  | "else" -> update lexbuf; ELSE
  | '#' -> update lexbuf; NUMBER_SIGN
  | '_' -> update lexbuf; WILDCARD
  | '"' -> update lexbuf; STRING (string (Buffer.create 0) lexbuf)
  | "{." -> update lexbuf; LEFT_BRACE
  | ".}" -> update lexbuf; RIGHT_BRACE
  | "{-" -> update lexbuf; read_comment 0 lexbuf
  | "--" -> update lexbuf; read_comment_line lexbuf
  | '[' -> update lexbuf; LEFT_BRACK
  | ']' -> update lexbuf; RIGHT_BRACK
  | '(' -> update lexbuf; LEFT_PAREN
  | ')' -> update lexbuf; RIGHT_PAREN
  | ',' -> update lexbuf; COMMA
  | ';' -> update lexbuf; SEMICOLON
  | '<' -> update lexbuf; LESS
  | '>' -> update lexbuf; MORE
  | '|' -> update lexbuf; BAR
  | '.' -> update lexbuf; DOT
  | '~' -> update lexbuf; NOT (lexeme lexbuf)
  | '+' -> update lexbuf; ADD (lexeme lexbuf)
  | '-' -> update lexbuf; SUB (lexeme lexbuf)
  | '=' -> update lexbuf; EQ (lexeme lexbuf)
  | ":=" -> update lexbuf; ASSIGN (lexeme lexbuf)
  | "/=" -> update lexbuf; NOT_EQ (lexeme lexbuf)
  | "<:" -> update lexbuf; LT (lexeme lexbuf)
  | ":>" -> update lexbuf; GT (lexeme lexbuf)
  | ">=" -> update lexbuf; GTE (lexeme lexbuf)
  | "<=" -> update lexbuf; LTE (lexeme lexbuf)
  | "&&" -> update lexbuf; AND (lexeme lexbuf)
  | "||" -> update lexbuf; OR (lexeme lexbuf)
  | '*' -> update lexbuf; MULT (lexeme lexbuf)
  | '/' -> update lexbuf; DIV (lexeme lexbuf)
  | '%' -> update lexbuf; MOD (lexeme lexbuf)
  | "**" -> update lexbuf; POW (lexeme lexbuf)
  | ':' -> update lexbuf; COLON (lexeme lexbuf)
  | integer -> update lexbuf; INT (Int.of_string (lexeme lexbuf))
  | float -> update lexbuf; FLOAT (Float.of_string (lexeme lexbuf))
  | id -> update lexbuf; IDENT (lexeme lexbuf)
  | _ -> assert false
and string buffer lexbuf =
  let store () = Buffer.add_string buffer (lexeme lexbuf) in
  let buf = lexbuf.stream in
  match%sedlex buf with
  | eof -> raise (SyntaxError "Unclosed string")
  | newline -> new_line lexbuf; store(); string buffer lexbuf
  | "\\\"" -> store(); string buffer lexbuf
  | "\\\\" -> store(); string buffer lexbuf
  | '\\' -> store(); string buffer lexbuf
  | '"' -> update lexbuf; Buffer.contents buffer
  | Plus (Compl ('"' | '\\' | '\r' | '\n')) -> store(); string buffer lexbuf
  | _ -> assert false
and read_comment level lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | "{-" -> read_comment (level + 1) lexbuf
  | "-}" ->
    if Int.equal level 0
    then token lexbuf
    else read_comment (level - 1) lexbuf
  | eof -> raise (SyntaxError ("Comment is not terminated"))
  | any -> read_comment level lexbuf
  | _ -> assert false
and read_comment_line lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | newline | eof -> token lexbuf
  | any -> read_comment_line lexbuf
  | _ -> assert false
