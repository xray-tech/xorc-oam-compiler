open! Core
open! Async

module type ModuleLoader = sig
  val load : string -> string option Deferred.t
  val all_modules : unit -> string list Deferred.t
end

let optional_file_contents path =
  Monitor.try_with (fun () -> Reader.file_contents path >>| Option.some )
  >>| function
  | Ok(v) -> v
  | Error(_) -> None

let fs path =
  (module struct
    let load mod_ =
      let mod_' = String.Search_pattern.(replace_all (create "\\.") ~in_:mod_ ~with_:"/") ^ ".orc" in
      optional_file_contents (Filename.concat path mod_')
    (* TODO nested directories *)
    let all_modules () =
      let%map dirs = Sys.ls_dir path in
      List.filter_map dirs ~f:(fun n ->
          match Filename.split_extension n with
          | (mod_, Some("orc")) -> Some(mod_)
          | _ -> None)
  end : ModuleLoader)

let multiloader loaders =
  (module struct
    let load mod_ = Deferred.List.find_map loaders ~f:(fun (module Loader : ModuleLoader) ->
        Loader.load mod_)

    let all_modules () =
      Deferred.List.concat_map loaders ~f:(fun (module Loader : ModuleLoader) ->
          Loader.all_modules ())
  end : ModuleLoader)

let empty_loader =
  (module struct
    let load _mod = return None
    let all_modules () = return []
  end : ModuleLoader)

let static_loader modules =
  (module struct
    let load mod_ = return (List.Assoc.find modules ~equal:String.equal mod_)
    let all_modules () = return (List.map modules ~f:Tuple2.get1)
  end : ModuleLoader)

let static_prelude = static_loader [%static_modules_dir "../../prelude"]

module List = struct
  include List
  let safe_sub l ~pos ~len =
    let llen = List.length l in
    List.sub l ~pos:(Int.min pos (llen - 1)) ~len:(Int.min len (llen - pos))
end

let annotate_code ?(before=5) ?(after=3) code line col msg =
  (* Including trailing newline *)
  let lines = String.split_on_chars code ~on:['\n'] in
  let from_line = Int.max (line - before) 0 in
  let indexed_lines = List.mapi lines ~f:(fun i x -> (Some(i), x)) in
  let msg' = String.(make col ' ' ^ "^--- " ^ msg) in
  let line_width = List.length lines |> Int.to_string |> String.length in
  let line_format = Scanf.format_from_string ("%" ^ Int.to_string line_width ^ "d: %s") "%d%s" in
  List.concat
    [List.sub indexed_lines ~pos:from_line ~len:((Int.min line before) + 1);
     [(None, msg')];
     List.safe_sub indexed_lines ~pos:(line + 1) ~len:after;]
  |> List.map ~f:(function
      | (Some(l), x) -> sprintf line_format l x
      | (None, x) -> sprintf "%s  %s" (String.make line_width ' ') x)
  |> String.concat ~sep:"\n"

let add_modules (module Loader : ModuleLoader) modules =
  let repository = Orcml.Repository.create () in
  let rec step = function
    | [] -> return (Ok repository)
    | mod_::xs ->
      match%bind Loader.load mod_ with
      | None -> Error(`CantLoadMod mod_) |> return
      | Some(code) ->
        match Orcml.add_module ~repository ~path:mod_ code with
        | Error _ as err -> return err
        | Ok(()) -> step xs in
  step modules

