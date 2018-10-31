open Base

let lexing_col {Lexing.pos_bol; pos_cnum} =
  pos_cnum - pos_bol

module Pos = struct
  type t = {
    path : string;
    line : int;
    col : int;
  }
  and range = {
    start : t;
    finish : t;
  } [@@deriving sexp, compare]

  let of_lexing ({ Lexing.pos_fname; pos_lnum } as pos) =
    { path = pos_fname;
      line = pos_lnum;
      col = lexing_col pos }

  let dummy = { path = ""; line = 0; col = 0 }

  let dummy_range = { start = dummy; finish = dummy }
end


type const =
  | Int of int
  | Float of float
  | String of string
  | Signal
  | Null
  | Bool of bool [@@deriving sexp, compare]

type ident = string [@@deriving sexp, compare]

type p' =
  | PVar of ident
  | PWildcard
  | PConst of const
  | PTuple of p list
  | PList of p list
  | PCons of p * p
  | PCall of ident * p list
  | PRecord of (string * p) list
  | PAs of p * string
  | PTyped of p * ty
and p = (p' * Pos.range) [@@deriving sexp_of, compare]
and ty' =
    | TyVar of string
  | TyApp of string * ty list
  | TyRecord of (string * ty) list
  | TyTuple of ty list
  | TyFun of tyfun
and tyfun = string list * ty list * ty
and ty = (ty' * Pos.range)

type e' =
  | EOtherwise of e * e
  | EParallel of e * e
  | EPruning of e * p * e
  | ESequential of e * p * e
  | EConst of const
  | EIdent of ident
  | EFieldAccess of e * string
  | ECall of e * ty list * e list
  | EFFC of string * e list
  | EStop
  | EList of e list
  | ETuple of e list
  | ERecord of (string * e) list
  | EDecl of decl * e
  | ECond of e * e * e
  | ELambda of string list * p list * ty option * e
  | EHasType of e * ty
  | EOverrideType of e * ty
  | EModule                         (* special mark for module end *)
and decl' =
  | DVal of p * e
  | DDef of ident * string list * p list * ty option * e option  * e
  | DSig of ident * tyfun
  | DSite of ident * string
  | DInclude of string
  | DData of string * string list * constructor list
  | DAlias of ident * string list * ty
  | DRefer of string * ident list
and constructor = ident * int
and decl = decl' * Pos.range
and e = e' * Pos.range [@@deriving sexp_of, compare]

exception UnsupportedImportType of string

let import_decl i n def = match i with
  | "site" -> Some (DSite (n, def))
  | _ -> None
