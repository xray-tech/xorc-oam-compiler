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
  let write_result r =
    Writer.write (Lazy.force Writer.stdout) (Orcml.Serialize.serialize_result r) in
  let rec server code state =
    let open Orcml_testkit in
    let module M = Message_pack in
    let input = (Lazy.force Reader.stdin) in
    Protocol.read_msg input  >>= function
    | Some(M.Int 0) ->
      (Protocol.read_msg input >>= function
        | None -> exit 0
        | Some(code) ->
          debug "----RUN";
          let code' = Orcml.Serialize.deserialize_bc code in
          (match Orcml.Inter.run code' with
           | Ok((_, _, _, state as v)) ->
             write_result v;
             server (Some code') (Some state)
           | Error(err) ->
             error "Runtime error:\n%s" (Error.to_string_hum err);
             exit 1))
    | Some(M.Int 1) when Option.is_empty code ->
      error "Unblock before code";
      exit 1
    | Some(M.Int 1) ->
      debug "---UNBLOCK";
      (Protocol.read_msg input >>= function
        | None -> exit 0
        | Some(M.List ((M.Int id)::vs)) ->
          let (v, _) = Orcml.Serialize.deserialize_value (fun _ -> assert false) vs in
          let res = Orcml.Inter.unblock (Option.value_exn code) (Option.value_exn state) id v in
          (match res with
           | Ok((_, _, _, state as v)) ->
             write_result v;
             server code (Some state)
           | Error(err) ->
             error "Runtime error:\n%s" (Error.to_string_hum err);
             exit 1)
        | _ -> error "Bad message"; exit 1)
    | None -> exit 0
    | _ -> error "Bad message"; exit 1 in
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
