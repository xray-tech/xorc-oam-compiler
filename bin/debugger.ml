open! Core
open! Async
open! Async_interactive
open! Log.Global

module Lib = Orcml_bin_lib
module D = Orcml.Debugger

let print_trace = function
  | D.PublishedValue v ->
    print_endline (sprintf "\nPublished: %s"
                     (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum))
  | D.NewThread id ->
    print_endline (sprintf "\nNew thread #%d" id)
  | D.HaltedThread id ->
    print_endline (sprintf "\nHalted #%d" id)
  | D.Coeffect { thread; id; desc; } ->
    print_endline (sprintf "\nCoeffect %d (thread #%d): %s" id thread
                     (Orcml.Value.to_string desc))
  | D.Error { thread; ffc; args } ->
    let args = List.map args ~f:(fun x -> Orcml.Value.to_string x)
               |> String.concat ~sep:", " in
    print_endline (sprintf "\nError (thread #%d) while executing `%s`(%s)"
                     thread ffc args)

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
      Some (sprintf "%s -> %s" ident (D.v_to_string v)) in
  Array.to_list env
  |> List.filter_map ~f:pair
  |> String.concat ~sep:"\n"

let print_thread loader prog {D.id; env; pos} =
  let%bind current_position = thread_position loader prog pos in
  let env = thread_env env in
  print_endline (sprintf "\nThread #%d\n\n%s\n%s\n" id current_position env)

let apply_coeffects coeffects = function
  | D.Coeffect { id } -> coeffects := id::!coeffects
  | _ -> ()

let with_cursor f =
  let input_fd = Fd.stdin () in
  let open Unix.Terminal_io in
  let%bind () = print_string "\027[?25h" in
  let%bind terminal = tcgetattr input_fd in
  terminal.c_echo <- true;
  terminal.c_icanon <- true;
  let%bind () = tcsetattr terminal input_fd ~mode:TCSANOW in
  let%bind res = f () in
  terminal.c_echo <- false;
  terminal.c_icanon <- false;
  let%bind () = tcsetattr terminal input_fd ~mode:TCSANOW in
  let%bind () = print_string "\027[?25l" in
  return res

let ask_string ~label input =
  with_cursor (fun () ->
      let%bind () = print_string label in
      match%bind Reader.read_line input with
      | `Eof -> return None
      | `Ok v ->
        Some v |> return)

let ask_int ~label input =
  match%map ask_string ~label input with
  | None -> `Eof
  | Some v -> try `Value (Int.of_string v) with
    | _ -> `BadFormat

let ask_value ~label input =
  match%map ask_string ~label input with
  | None -> `Eof
  | Some v -> match Orcml.parse_value v with
    | Ok v -> `Value v
    | Error _ -> `BadFormat

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
  let coeffects = ref [] in
  let i = ref 0 in
  let breakpoints = ref [] in
  let apply_result prev_threads (active_threads, trace) =
    let%bind () = Deferred.List.iter trace ~f:print_trace in
    threads := active_threads @ prev_threads;
    List.iter trace ~f:(apply_coeffects coeffects);
    return () in
  let check_breakpoints {D.pos = {path; line}} =
    List.find !breakpoints ~f:(fun (path', line') ->
        String.equal path path' && Int.equal line line')
    |> Option.is_some in
  let rec resume () =
    match !threads with
    | [] -> return ()
    | thread::threads' ->
      i := !i + 1;
      let (active_threads, trace) = D.tick state thread in
      let%bind () = apply_result threads' (active_threads, trace) in
      if List.find active_threads ~f:check_breakpoints |> Option.is_some
      then return ()
      else resume () in
  let execute char =
    match char with
    | 's' ->
      (match !threads with
       | [] -> print_endline "\nNo more active threads to step"
       | thread::threads' ->
         i := !i + 1;
         D.tick state thread |> apply_result threads')
    | 'p' ->
      Deferred.List.iter !threads ~f:(print_thread loader prog)
    | 'c' ->
      (match%bind ask_int ~label:"\nCoeffect: " input with
       | `Eof -> return ()
       | `BadFormat -> print_endline "\nBad formatted id"
       | `Value id ->
         match%bind ask_value ~label:"\nCoeffect value: " input with
         | `Eof -> return ()
         | `BadFormat -> print_endline "\nBad formatted value"
         | `Value v ->
           coeffects := List.filter !coeffects ~f:(fun id' -> not (Int.equal id id'));
           match D.unblock state id v with
           | None -> print_endline "\nUnknown coeffect"
           | Some res ->
             i := !i + 1;
             apply_result !threads res)
    | 'r' ->
      resume ()
    | 'b' ->
      (match%bind ask_string ~label:"\nModule path (empty for main program): " input with
       | None -> return ()
       | Some path ->
         match%bind ask_int ~label:"\nLine number: " input with
         | `Eof -> return ()
         | `BadFormat -> print_endline "\nBad formatted line number"
         | `Value line ->
           breakpoints := (path, line - 1)::!breakpoints; return ())
    | _ -> print_endline "\nUnknown command" in
  let rec step () =
    let%bind () = print_string (sprintf "\r\027[1K> s(tep) | p(rint) | c(offect) | b(reakpoint) | r(esume) [step %d] [active coeffects %d]" !i (List.length !coeffects)) in
    match%bind Reader.read_char input with
    | `Eof -> return ()
    | `Ok char ->
      let%bind () = execute char in
      if List.length !threads > 0 || not (List.is_empty !coeffects)
      (* TODO add killed coeffects checking*)
      then step ()
      else print_endline "\nExecution finished" in
  step ()
