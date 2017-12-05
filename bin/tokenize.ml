open Base
open Lexing

module Parser = Orcml.Parser
module Lexer = Orcml_lexer

let sprintf = Printf.sprintf

let to_string token =
  let open Parser in
  match token with
  | EOF -> "EOF"
  | INT i -> sprintf "INT(%d)" i
  | IDENT s -> sprintf "IDENT(%s)" s
  | FLOAT f -> sprintf "FLOAT(%f)" f
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | SIGNAL -> "SIGNAL"
  | NULL -> "NULL"
  | STOP -> "STOP"
  | VAL -> "VAL"
  | REFER -> "REFER"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | DEF -> "DEF"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | ADD _ -> "ADD"
  | SUB _ -> "SUB"
  | MULT _ -> "MULT"
  | DIV _ -> "DIV"
  | MOD _ -> "MOD"
  | POW _ -> "POW"
  | LT _ -> "LT"
  | GT _ -> "GT"
  | LTE _ -> "LTE"
  | GTE _ -> "GTE"
  | EQ _ -> "EQ"
  | NOT_EQ _ -> "NOT_EQ"
  | NOT _ -> "NOT"
  | COLON _ -> "COLON"
  | DOT -> "DOT"
  | NUMBER_SIGN -> "NUMBER_SIGN"
  | DEREFERENCE _ -> "DEREFERENCE"
  | ASSIGN _ -> "ASSIGN"
  | WILDCARD -> "WILDCARD"
  | TYPE -> "TYPE"
  | SEMICOLON -> "SEMICOLON"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | MORE -> "MORE"
  | LESS -> "LESS"
  | LEFT_BRACK -> "LEFT_BRACK"
  | RIGHT_BRACK -> "RIGHT_BRACK"
  | LAMBDA -> "LAMBDA"
  | INCLUDE -> "INCLUDE"
  | IMPORT -> "IMPORT"
  | BAR -> "BAR"
  | AS -> "AS"
  | STRING s -> sprintf "STRING(%s)" s
  | OR _ -> "OR"
  | AND _ -> "AND"

let () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_channel Stdio.stdin) in
  let rec loop acc =  function
    | Parser.EOF   ->  to_string Parser.EOF :: acc |> List.rev
    | x     ->  loop (to_string x :: acc) (Lexer.token lexbuf)
  in
  loop [] (Lexer.token lexbuf)
  |> String.concat ~sep:" "
  |> Stdio.print_endline
