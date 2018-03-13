open! Core
open! Async
open! Async_interactive
open! Log.Global

module D = Orcml.Debugger

let print_trace = function
  | D.PublishedValue v ->
    print_endline (sprintf "Published: %s"
                     (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum))
  | D.NewThread id ->
    print_endline (sprintf "New thread #%d" id)
  | D.HaltedThread id ->
    print_endline (sprintf "Halted #%d" id)
  | D.Coeffect { thread; id; desc; } ->
    print_endline (sprintf "Coeffect %d (thread #%d): %s" id thread
                     (Orcml.Value.sexp_of_t desc |> Sexp.to_string_hum))
  | D.Error { thread; ffi; args } ->
    let args = List.map args ~f:(fun x -> Orcml.Value.sexp_of_t x |> Sexp.to_string_hum)
               |> String.concat ~sep:", " in
    print_endline (sprintf "Error (thread #%d) while executing `%s`(%s)"
                     thread ffi args)

let thread_position (module Loader : Lib.ModuleLoader) prog {Orcml.line; col; path} =
  let annotate prog = Lib.annotate_code ~before:1 ~after:1 prog line col "" in
  match path with
  | "" ->
    return (annotate prog)
  | _ -> (match%map Loader.load path with
      | Some(v) -> sprintf "Module %s:\n%s" path (annotate v)
      | None -> "<MISSING SOURCE CODE>")

let thread_env env =
  let pair = function
    | (D.Var.Generated _, _) -> None
    | (D.Var.Handcrafted {ident}, v) ->
      Some (sprintf "%s -> %s" ident (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum)) in
  Array.to_list env
  |> List.filter_map ~f:pair
  |> String.concat ~sep:"\n"

let print_thread loader prog {D.id; env; pos} =
  let%bind current_position = thread_position loader prog pos in
  let env = thread_env env in
  print_endline (sprintf "\nThread #%d\n\n%s\n%s\n" id current_position env)

let execute loader prog state threads = function
  | 's' ->
    (match !threads with
     | [] -> assert false
     | thread::threads' ->
       let (active_threads, trace) = D.tick state thread in
       (* let%bind () = Deferred.List.iter trace ~f:print_trace in *)
       threads := active_threads @ threads';
       return ())
  | 'p' ->
    Deferred.List.iter !threads ~f:(print_thread loader prog)
  | _ ->print_endline "Unknown command"

let run loader prog inter =
  let input = Lazy.force Reader.stdin in
  let%bind () = print_string "\027[?25l" in
    Shutdown.at_shutdown (fun () ->
      print_string "\027[?25h"
    );
  let input_fd = Fd.stdin () in
  let%bind terminal = Unix.Terminal_io.tcgetattr input_fd in
  terminal.Unix.Terminal_io.c_echo <- false;
terminal.Unix.Terminal_io.c_icanon <- false;
  let%bind () = Unix.Terminal_io.tcsetattr terminal input_fd ~mode:Unix.Terminal_io.TCSANOW in
  let (state, init_threads) = D.init inter in
  let threads = ref init_threads in
  let rec step i =
    let%bind () = print_string (sprintf "\r\027[1K> [tick %d] s(tep) | p(rint)" i) in
    match%bind Reader.read_char input with
    | `Eof -> return ()
    | `Ok char ->
      let%bind () = execute loader prog state threads char in
      if List.length !threads > 0
      then step (i + 1)
      else print_endline "Execution finished" in
  step 0
