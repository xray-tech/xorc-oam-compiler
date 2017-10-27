open Core

module Lexing = struct
  include Lexing

  let sexp_of_position {pos_bol; pos_lnum; pos_fname; pos_cnum} =
    let open Sexp in
    List [Atom "pos";
          Atom pos_fname;
          Atom (string_of_int pos_lnum);
          Atom (string_of_int (pos_cnum - pos_bol))]
end

type pos = {
  pstart : Lexing.position;
  pend : Lexing.position;
} [@@deriving sexp_of]

let dummy = let z = Lexing.{ pos_bol = 0;
                             pos_lnum = 0;
                             pos_fname = "%";
                             pos_cnum = 0 } in
  { pstart = z; pend = z }

type p' =
  | PVar of string
  | PWildcard
  | PTuple of p list
  | PList of p list
  | PCons of p * p
  | PAs of p * string
  | PTyped of p * ty
and ty =
  | TyVar of string
  | TyApp of string * ty list
  | TyRecord of (string * ty) list
  | TyFun of tfun
and tfun = (string list * p list * ty)
and p = (p' * pos) [@@deriving sexp_of]

type unop =
  | OpNot
  | OpNeg
  | OpDeref [@@deriving sexp]

type const =
  | Int of int
  | Float of float
  | String of string [@@deriving sexp]

type ident = string [@@deriving sexp]

type e' =
  | EHasType of e * ty
  | EOtherwise of e * e
  | EParallel of e * e
  | EPruning of e * p * e
  | ESequential of e * p * e
  | EConst of const
  | EIdent of ident
  | EFieldAccess of e * string
  | ECall of e * ty list * e list
  | EStop
  | EList of e list
  | ETuple of e list
  | ERecord of (string * e) list
  | EDecls of decl list * e
  | ECond of e * e * e
  | ELambda of string list * p list * ty option * e option * e
and decl' =
  | DVal of p * e
  | DSig of string * tfun
  | DDef of string * tfun
  | DSite of string * string
  | DInclude of string * decl list
  | DDatatype of string * string list * constructor list
  | DAlias of string * ty
  | DTyImport of string * string
and constructor = string * ty option list
and decl = decl' * pos
and e = e' * pos [@@deriving sexp_of]
