open Common

type unit = {
  is_closure : bool;
  orc_module : string;
  ident : string;
  ctx : ctx option ref;                (* ref here is only for recusrive defenitions. ctx of unit could contain unit itself *)
  params : Ir1.Var.t list;
  body : Ir1.e }
and binding =
  | BindVar
  | BindCoeffect
  | BindFun of unit
  | BindMod of (string * string)
and ctx = (string * binding) list [@@deriving sexp_of]

type t = unit list ref

let create () = ref []

let get t module_ fun_ =
  List.find !t ~f:(fun {orc_module = module_'; ident = ident'} ->
      String.equal module_' module_ && String.equal ident' fun_)

let set t unit =
  t := unit::!t
