open! Core
open! Async

module List = struct
  include List
  let safe_sub l ~pos ~len =
    let llen = List.length l in
    List.sub l ~pos:(Int.min pos (llen - 1)) ~len:(Int.min len (llen - pos))
end

module type ModuleLoader = sig
  val load : string -> string option Deferred.t
  val all_modules : unit -> string list Deferred.t
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
