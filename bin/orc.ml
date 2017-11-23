open! Core
open! Async
open! Log.Global

let set_logging verbose =
  (if verbose then set_level `Debug);
  let output = (Async_extended.Extended_log.Console.output (Lazy.force Writer.stderr)) in
  set_output [output]

let verbose_flag =
  let open Command.Param in
  flag "verbose" no_arg ~doc:"Verbose logging"

let compile =
  let open Command.Let_syntax in
  Command.basic'
    ~summary: "produce bytecode"
    [%map_open
      let input = flag "-i" (optional file) ~doc:"Orc file to compile. By default reads from stdin"
      and verbose = verbose_flag in
      fun () ->
        print_endline "haha"]

let read_file_or_stdin = function
  | None -> Reader.contents (Lazy.force Reader.stdin)
  | Some(f) -> Reader.file_contents f

let generate_result state_path (values, coeffects, _, s) =
  List.iter values (fun v ->
      info "Value: %s" (Orcml.Inter.sexp_of_v v |> Sexp.to_string_hum));
  List.iter coeffects (fun (id, v) ->
      info "Coeffect: %i -> %s" id (Orcml.Inter.sexp_of_v v |> Sexp.to_string_hum));
  List.iter s.Orcml.Inter.blocks (fun (id, v) ->
      info "Block: %i" id);
  exit 0

let exec =
  let open Command.Let_syntax in
  Command.basic'
    ~summary: "executes orc"
    [%map_open
      let input = anon (maybe ("INPUT" %: file))
      and bc = flag "-bc" no_arg ~doc:"Execute bytecode, not Orc source file. By default reads from stdin"
      and state = flag "-state" (optional file) ~doc:"Path to store intermediate state if any"
      and verbose = verbose_flag in
      let exec () =
        read_file_or_stdin input >>= fun prog ->
        let res = Orcml_syntax.Syntax.from_string prog
                  |> Result.bind ~f:Orcml.Ir1.translate
                  |> Result.bind ~f:Orcml.Compiler.compile in
        (match res with
         | Error(err) ->
           error "Can't compile: %s" (Orcml.Errors.format err);
           exit 1
         | Ok(compiled) ->
           debug "Compiled:\n%s" (Orcml.Inter.sexp_of_code compiled |> Sexp.to_string_hum);
           (match Orcml.Inter.run compiled with
            | Ok(v) -> generate_result state v
            | Error(err) ->
              error "Runtime error:\n%s" (Error.to_string_hum err);
              exit 1)) in
      fun () ->
        set_logging verbose;
        exec () |> ignore;
        Scheduler.go () |> never_returns
    ]

let tests_server =
  let write v = Writer.write (Lazy.force Writer.stdout) v in
  let write_result r =
    write (Orcml.Serialize.serialize_result r) in
  let rec server code state =
    let open Orcml_testkit in
    let module M = Message_pack in
    let input = (Lazy.force Reader.stdin) in
    Protocol.read_msg_or_exit ~code:0 input >>= function
    | M.Int 0 ->
      Protocol.read_msg_or_exit ~code:0 input >>= fun code ->
      let code' = Orcml.Serialize.deserialize_bc code in
      handle_res (Some code') (Orcml.Inter.run code')
    | M.Int 1 when Option.is_empty code ->
      error "Unblock before code";
      exit 1
    | M.Int 1 ->
      (Protocol.read_msg_or_exit ~code:0 input >>= function
        | M.List ((M.Int id)::vs) ->
          let (v, _) = Orcml.Serialize.deserialize_value (fun _ -> assert false) vs in
          let res = Orcml.Inter.unblock (Option.value_exn code) (Option.value_exn state) id v in
          handle_res code res
        | _ -> error "Bad message"; exit 1)
    | M.Int 2 ->
      (Protocol.read_msg_or_exit ~code:0 input >>= function
        | M.Int n ->
          Protocol.read_msg_or_exit ~code:0 input >>= fun code ->
          let code' = Orcml.Serialize.deserialize_bc code in
          (match Benchmark.latency1 ~style:Benchmark.Nil
                   (Int64.of_int n) Orcml.Inter.run code' with
           | [(_, [{Benchmark.wall}])] ->
             write (M.Float (wall *. 1000.0) |> M.String.to_string);
             server None None
           | _ -> assert false)
        | _ -> error "Bad message"; exit 1)
    | _ -> error "Bad message"; exit 1
  and handle_res code = function
    | Ok((_, _, _, state as v)) ->
      write_result v;
      server code (Some state)
    | Error(err) ->
      error "Runtime error:\n%s" (Error.to_string_hum err);
      exit 1 in
  let open Command.Let_syntax in
  Command.basic'
    ~summary: "tests-server. supports TestKit protocol"
    [%map_open
      let verbose = verbose_flag in
      fun () ->
        set_logging verbose;
        (Monitor.try_with_or_error (fun () -> server None None)
         >>= function
         | Ok(()) -> Async.return ()
         | Error(err) ->
           error "Unknown error while tests run:\n%s\n" (Error.to_string_hum err);
           exit 1)|> don't_wait_for;
        Scheduler.go () |> never_returns
    ]


let () =
  Command.group ~summary:"Orc programming language compiler and VM"
    [("compile", compile);
     ("exec", exec);
     ("tests-server", tests_server)]
  |> Command.run
