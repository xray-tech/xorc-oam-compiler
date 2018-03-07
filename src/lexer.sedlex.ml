open Common
open Parser

type lexbuf = {
  buf: Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(path="") buf =
  let pos = {Lexing.
              pos_fname = path;
              pos_lnum = 0;
              pos_bol = 0;
              pos_cnum = 0}
  in { pos ; buf}

(** Update the position with the stream. *)
let integer = [%sedlex.regexp? ('1'..'9', Star ('0'..'9')) | '0']

let digit = [%sedlex.regexp? '0'..'9']
let frac = [%sedlex.regexp? ('.', Star digit)]
let exp = [%sedlex.regexp? (('e' | 'E'), Opt ('-' | '+'), Plus digit)]

let float = [%sedlex.regexp? (Star digit, Opt frac, Opt exp)]

let newline = [%sedlex.regexp? ('\n' | '\r' | "\r\n")]

let id = [%sedlex.regexp? (('a'..'z' | 'A'..'Z' | '_'), Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9' | '\''))]

let rec token on_comment ({ buf } as lexbuf) =
  with_return (fun r ->
      let lexeme () = Sedlexing.Utf8.lexeme buf in
      let store buffer = Buffer.add_string buffer (lexeme ()) in
      let start = ref None in
      let pos_with_start () =
        { lexbuf.pos with Lexing.pos_cnum = Sedlexing.lexeme_start buf } in
      let set_start () =
        start := Some (pos_with_start ()) in
      let new_line () =
        let open Lexing in
        let lcp = lexbuf.pos in
        lexbuf.pos <-
          {lcp with
           pos_lnum = lcp.pos_lnum + 1;
           pos_bol = Sedlexing.lexeme_end buf;
          } in
      let rec step () =
        match%sedlex buf with
        | Plus (Chars " \t\r") -> step ()
        | newline -> new_line (); step ()
        | eof -> EOF
        | "true" -> TRUE
        | "false" -> FALSE
        | "null" -> NULL
        | "signal" -> SIGNAL
        | "stop" -> STOP
        | "type" -> TYPE
        | "val" -> VAL
        | "refer from" -> REFER
        | "import" -> IMPORT
        | "include" -> INCLUDE
        | "lambda" -> LAMBDA
        | "as" -> AS
        | "def" -> DEF
        | "sig" -> SIG
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | '#' -> NUMBER_SIGN
        | '_' -> WILDCARD
        | '"' -> set_start (); STRING (string (Buffer.create 0))
        | "{." -> LEFT_BRACE
        | ".}" -> RIGHT_BRACE
        | "{-" -> comment_token (fun () -> read_comment (Buffer.create 0) 0)
        | "--" -> comment_token (fun () -> read_comment_line (Buffer.create 0))
        | "::" -> DOUBLE_COLON
        | ":!:" -> TOVERRIDE
        | '[' -> LEFT_BRACK
        | ']' -> RIGHT_BRACK
        | '(' -> LEFT_PAREN
        | ')' -> RIGHT_PAREN
        | ',' -> COMMA
        | ';' -> SEMICOLON
        | '<' -> LESS
        | '>' -> MORE
        | '|' -> BAR
        | '.' -> DOT
        | '~' -> NOT (lexeme ())
        | '?' -> DEREFERENCE (lexeme ())
        | '+' -> ADD (lexeme ())
        | '-' -> SUB (lexeme ())
        | '=' -> EQ (lexeme ())
        | ":=" -> ASSIGN (lexeme ())
        | "/=" -> NOT_EQ (lexeme ())
        | "<:" -> LT (lexeme ())
        | ":>" -> GT (lexeme ())
        | ">=" -> GTE (lexeme ())
        | "<=" -> LTE (lexeme ())
        | "&&" -> AND (lexeme ())
        | "||" -> OR (lexeme ())
        | '*' -> MULT (lexeme ())
        | '/' -> DIV (lexeme ())
        | '%' -> MOD (lexeme ())
        | "**" -> POW (lexeme ())
        | ':' -> COLON (lexeme ())
        | '`' -> set_start (); FFI (ffi (Buffer.create 0))
        | integer -> INT (Int.of_string (lexeme ()))
        | float -> FLOAT (Float.of_string (lexeme ()))
        | id -> IDENT (lexeme ())
        | _ -> assert false
      and string buffer =
        match%sedlex buf with
        | eof -> r.return (Result.Error (pos_with_start (), "Unclosed string"))
        | newline -> new_line (); store buffer; string buffer
        | '\\' ->  escaped_char buffer
        | '"' -> Buffer.contents buffer
        | Plus (Compl ('"' | '\\' | '\r' | '\n')) -> store buffer; string buffer
        | _ -> assert false
      and escaped_char buffer =
        match%sedlex buf with
        | eof -> r.return (Error (pos_with_start (), "Unexpected end of input"))
        | '"' -> store buffer; string buffer
        | _ -> r.return (Error (pos_with_start (),  "Invalid escape sequence"))
      and ffi buffer =
        match%sedlex buf with
        | eof | newline -> r.return (Error (pos_with_start (),  "Unclosed ffi"))
        | '`' -> Buffer.contents buffer
        | Plus (Compl ('`' | '\n')) -> store buffer; ffi buffer
        | _ -> assert false
      and read_comment buffer level =
        match%sedlex buf with
        | "{-" -> store buffer; read_comment buffer (level + 1)
        | "-}" ->
          if Int.equal level 0
          then (Buffer.contents buffer)
          else (store buffer; read_comment buffer (level - 1))
        | eof -> r.return (Error (pos_with_start (),  "Comment is not terminated"))
        | newline -> new_line (); store buffer; read_comment buffer level
        | any -> store buffer; read_comment buffer level
        | _ -> assert false
      and read_comment_line buffer =
        match%sedlex buf with
        | newline | eof -> Buffer.contents buffer
        | any -> Buffer.add_string buffer (lexeme ()); read_comment_line buffer
        | _ -> assert false
      and comment_token reader =
        let start = lexbuf.pos in
        let comment = reader () in
        let end_ = lexbuf.pos in
        let pos = Ast.Pos.{ start = of_lexing start;
                            finish = of_lexing end_ } in
        on_comment comment pos;
        step () in
      let token = step () in
      let start' = match !start with
        | Some v -> v
        | None -> pos_with_start () in
      lexbuf.pos <- { lexbuf.pos with Lexing.pos_cnum = Sedlexing.lexeme_end buf };
      Ok (token, start', lexbuf.pos))
