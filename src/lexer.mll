{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id       { IDENT (Lexing.lexeme lexbuf) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "nil"    { NIL }
  | "signal" { SIGNAL }
  | "stop"   { STOP }
  | "type"   { TYPE }
  | "val"    { VAL }
  | "import" { IMPORT }
  | "include" { INCLUDE }
  | "lambda" { LAMBDA }
  | "as"     { AS }
  | "def"    { DEF }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | '_'      { WILDCARD }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | "::"     { DOUBLE_COLON }
  | ":!:"    { TOVERRIDE }
  | "{."     { LEFT_BRACE }
  | ".}"     { RIGHT_BRACE }
  | "{-"     { LEFT_COMMENT }
  | "-}"     { RIGHT_COMMENT }
  | "--"     { COMMENT }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | ','      { COMMA }
  | '?'      { DEREFERENCE }
  | '+'      { ADD (Lexing.lexeme lexbuf) }
  | '-'      { SUB (Lexing.lexeme lexbuf) }
  | '='      { EQ (Lexing.lexeme lexbuf) }
  | ';'      { SEMICOLON }
  | '<'      { LESS }
  | '>'      { MORE }
  | '|'      { BAR }
  | ":="      { ASSIGN (Lexing.lexeme lexbuf) }
  | "\\="      { NOT_EQ (Lexing.lexeme lexbuf) }
  | "<:"      { LT (Lexing.lexeme lexbuf) }
  | ":>"      { GT (Lexing.lexeme lexbuf) }
  | ">="      { GTE (Lexing.lexeme lexbuf) }
  | "<="      { LTE (Lexing.lexeme lexbuf) }
  | "&&"      { AND (Lexing.lexeme lexbuf) }
  | "||"      { OR (Lexing.lexeme lexbuf) }
  | '*'      { MULT (Lexing.lexeme lexbuf) }
  | '/'      { DIV (Lexing.lexeme lexbuf) }
  | '%'      { MOD (Lexing.lexeme lexbuf) }
  | "**"      { POW (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
