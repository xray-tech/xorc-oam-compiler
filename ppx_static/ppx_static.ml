open Base
open Ppxlib

let read_modules dir =
  if not (Caml.Sys.file_exists dir)
  || not (Caml.Sys.is_directory dir)then
    []
  else
    Caml.Sys.readdir dir
    |> Array.to_list
    |> List.filter_map ~f:(fun n ->
        match Caml.Filename.extension n with
        | ".orc" ->
          let path = Caml.Filename.concat dir n in
          Some((Caml.Filename.chop_extension n,
                Stdio.In_channel.read_all path))
        | _ -> None)

let static =
  Extension.declare "static_modules_dir" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~loc ~path:_ dir ->
       let modules = read_modules dir in
       let open Ast_builder.Default in
       elist ~loc (List.map modules ~f:(fun (m, v) ->
           pexp_tuple ~loc [estring ~loc m; estring ~loc v])))

let () =
  Driver.register_transformation "static" ~extensions: [static]
