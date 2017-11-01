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

type const =
  | Int of int
  | Float of float
  | String of string
  | Signal
  | Null
  | Bool of bool [@@deriving sexp]

type ident = string [@@deriving sexp]

type p' =
  | PVar of ident
  | PWildcard
  | PConst of const
  | PTuple of p list
  | PList of p list
  | PCons of p * p
  | PRecord of (string * p) list
  | PAs of p * string
  | PTyped of p * ty
and ty' =
  | TyVar of string
  | TyApp of string * ty list
  | TyRecord of (string * ty) list
  | TyTuple of ty list
  | TyFun of tyfun
and tyfun = string list * ty list * ty
and ty = (ty' * pos)
and p = (p' * pos) [@@deriving sexp_of]

type e' =
  | EHasType of e * ty
  | EOverrideType of e * ty
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
  | EDecl of decl * e
  | ECond of e * e * e
  | ELambda of string list * p list * ty option * e
and decl' =
  | DVal of p * e
  | DSig of ident * tyfun
  | DDef of ident * string list * p list * ty option * e option  * e
  | DSite of ident * string
  | DInclude of string
  | DData of string * string list * constructor list
  | DAlias of ident * string list * ty
  | DTyImport of string * string
and constructor = string * ty option list
and decl = decl' * pos
and e = e' * pos [@@deriving sexp_of]

exception UnsupportedImportType of string

let import_decl i n def = match i with
  | "site" -> Some (DSite (n, def))
  | _ -> None
