open Base

type prim = Let [@@deriving compare, enumerate]

exception UnboundVar of string

let list_index l v ~f =
  let rec index acc = function
    | [] -> None
    | x::xs when f v x = 0 -> Some(acc)
    | _::xs -> index (acc + 1) xs in
  index 0 l

let rec ctx_index ctx ident =
  match list_index ctx ident String.compare with
  | None -> raise (UnboundVar ident)
  | Some i -> i

let prim_index p =
  match list_index all_of_prim p compare_prim with
  | Some i -> i
  | None -> assert false

let prim_target p = Inter.TPrim(prim_index p)

type state = { ctx : string list;
               mutable code : (int * Inter.t list) list }

type fun_state = { mutable e_code : Inter.t list;
                   mutable max_stack : int }

let set_max_stack fun_state ctx =
  fun_state.max_stack <- Int.max (List.length ctx) fun_state.max_stack

let rec compile_e fun_state ({ ctx } as state) (e, pos) =
  set_max_stack fun_state ctx;
  let open Ast in
  let op = match e with
    | EIdent x -> Inter.Call(prim_target Let, [| (ctx_index ctx x) |])
    | EConst v -> Inter.Const v
    | EParallel(e1, e2) ->
      let e1' = compile_e fun_state state e1 in
      let e2' = compile_e fun_state state e2 in
      Inter.Parallel(e1', e2')
    | EPruning(e1, ((PVar v), _), e2) ->
      let state' = { state with ctx = v::ctx } in
      let e1' = compile_e fun_state state' e1 in
      let e2' = compile_e fun_state state e2 in
      Inter.Pruning(e1', List.length ctx, e2')
    | EDecl((DVal(((PVar v), _), val_e), _), e) ->
      compile_e fun_state state ((EPruning(e, p, val_e)), pos) in
  let i = List.length fun_state.e_code in
  fun_state.e_code <- op::fun_state.e_code;
  i

let compile_fun state params body =
  let i = List.length state.code in
  let fun_state = { e_code = []; max_stack = 0 } in
  compile_e fun_state state body |> ignore;
  state.code <- (fun_state.max_stack, fun_state.e_code)::state.code;
  i

let compile e =
  let state = { ctx = []; code = [] } in
  compile_fun state [] e |> ignore;
  Array.of_list_rev_map state.code ~f:(fun (s, code) ->
      (s, Array.of_list_rev code))
