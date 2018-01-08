open Base

module Lexing = struct
  include Lexing

  let sexp_of_position {pos_bol; pos_lnum; pos_fname; pos_cnum} =
    let open Sexp in
    List [Atom "pos";
          Atom pos_fname;
          Atom (Int.to_string (pos_lnum + 1));
          Atom (Int.to_string (pos_cnum - pos_bol))]

  let compare_position a b =
    let {pos_bol; pos_lnum; pos_fname; pos_cnum} = a in
    let a' = (pos_fname, pos_bol, pos_lnum, pos_cnum) in
    let {pos_bol; pos_lnum; pos_fname; pos_cnum} = b in
    let b' = (pos_fname, pos_bol, pos_lnum, pos_cnum) in
    [%compare: string * int * int * int] a' b'
end

type pos = {
  pstart : Lexing.position;
  pend : Lexing.position;
} [@@deriving sexp_of, compare]

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
and p = (p' * pos) [@@deriving sexp_of, compare]
and ty' =
    | TyVar of string
  | TyApp of string * ty list
  | TyRecord of (string * ty) list
  | TyTuple of ty list
  | TyFun of tyfun
and tyfun = string list * ty list * ty
and ty = (ty' * pos)

type e' =
  | EOtherwise of e * e
  | EParallel of e * e
  | EPruning of e * p * e
  | ESequential of e * p * e
  | EConst of const
  | EIdent of ident
  | EFieldAccess of e * string
  | ECall of e * ty list * e list
  | EFFI of string * e list
  | EStop
  | EList of e list
  | ETuple of e list
  | ERecord of (string * e) list
  | EDecl of decl * e
  | ECond of e * e * e
  | ELambda of string list * p list * ty option * e
  | EHasType of e * ty
  | EOverrideType of e * ty
  | ENS                         (* special mark for Namespace end *)
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
and decl = decl' * pos
and e = e' * pos [@@deriving sexp_of, compare]

exception UnsupportedImportType of string

let import_decl i n def = match i with
  | "site" -> Some (DSite (n, def))
  | _ -> None
