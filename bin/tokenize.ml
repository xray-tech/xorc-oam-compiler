open Core
open Lexing

module Parser = Orcml.Parser
module Lexer = Orcml.Lexer

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
  | NIL -> "NIL"
  | STOP -> "STOP"
  | VAL -> "VAL"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | DEF -> "DEF"
  | DOUBLE_COLON -> "DOUBLE_COLON"
  | TOVERRIDE -> "TOVERRIDE"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | LEFT_COMMENT -> "LEFT_COMMENT"
  | RIGHT_COMMENT -> "RIGHT_COMMENT"
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
  | DEREFERENCE -> "DEREFERENCE"
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
  | COMMENT -> "COMMENT"
  | BAR -> "BAR"
  | AS -> "AS"
  | STRING s -> sprintf "STRING(%s)" s
  | OR _ -> "OR"
  | AND _ -> "AND"
  

let () =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  let rec loop acc =  function
    | Parser.EOF   ->  to_string Parser.EOF :: acc |> List.rev
    | x     ->  loop (to_string x :: acc) (Lexer.read lexbuf)
  in
  loop [] (Lexer.read lexbuf)
  |> String.concat ~sep:" "
  |> print_endline
