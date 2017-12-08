open! Core
open! Async
open! Log.Global

module Lexer = Orcml_lexer
open Orcml_testkit

type results = { mutable success : int;
                 mutable failed : int }

let strings_to_values vs =
  List.map vs Orcml.parse_value
  |> List.map ~f:(fun v -> Or_error.ok_exn v)

let values_to_set = Set.of_list ~comparator:Orcml.Value.comparator

let rec print_stderr p =
  Reader.read_line p >>= function
  | `Eof -> return ()
  | `Ok(l) ->
    error "Engine stderr: %s" l;
    print_stderr p

module T = Orcml.Testkit
module Serializer = T.Serializer

let run_loop p fail compiled checks =
  let input = Process.stdin p in
  let output = Process.stdout p in

  Serializer.dump_msg (T.Execute compiled) |> Protocol.write input;

  let check_values check actual =
    match check with
    | Tests.AllOf expected ->
      let actual' = values_to_set actual in
      let expected' = strings_to_values expected in
      let expected'' = values_to_set expected' in
      if Set.equal actual' expected'' then
        `Ok
      else
        `Fail (sprintf "expected: %s\nactual: %s"
                 ([%sexp_of: Orcml.Value.t list] expected' |> Sexp.to_string_hum)
                 ([%sexp_of: Orcml.Value.t list] actual |> Sexp.to_string_hum))
    | OneOf expected ->
      let expected' = strings_to_values expected in
      let expected'' = values_to_set expected' in
      (match actual with
       | [v] ->
         if (Set.mem expected'' v) then
           `Ok
         else
           `Fail (sprintf "expected one of: %s\nactual: %s"
                    ([%sexp_of: Orcml.Value.t list] expected' |> Sexp.to_string_hum)
                    ([%sexp_of: Orcml.Value.t] v |> Sexp.to_string_hum))
       | vals ->
         `Fail (sprintf "expected only one of %s\nactual: %s"
                  ([%sexp_of: Orcml.Value.t list] expected' |> Sexp.to_string_hum)
                  ([%sexp_of: Orcml.Value.t list] vals |> Sexp.to_string_hum))) in
  let rec tick check =
    let%bind {T.values = actual; killed} = Protocol.read_res output in
    match check with
    | Tests.Check expected ->
      check_values expected actual |> return
    | Tests.CheckAndResume { values; unblock = (id, v); next } ->
      (match check_values values actual with
       | `Ok ->
         let v = Orcml.parse_value v |> Or_error.ok_exn in
         T.Continue(id, v) |> Serializer.dump_msg |> Protocol.write input;
         tick next
       | other -> return other)
  in
  tick checks

let run_test results p (e, checks) =
  debug "Run test: %s" e;
  let res =
    let open Or_error.Let_syntax in
    let%bind parsed = Orcml.parse e in
    let%bind ir = Orcml.translate_no_deps parsed in
    Orcml.compile ir in
  let fail reason =
    results.failed <- results.failed + 1;
    error "Test program:\n%s\nFailed with error: %s\n\n" e reason in
  match res with
  | Ok((_, repo)) ->
    run_loop p fail (Orcml.finalize repo) checks >>| (function
        | `Ok -> results.success <- results.success + 1
        | `Fail reason ->
          fail reason)
  | Error(err) ->
    fail (Error.to_string_hum err); return ()

let with_prog (prog, args) tests f =
  let tests' = List.concat_map tests (fun (_, l) -> l) in
  let open Async.Let_syntax in
  match%bind Process.create prog args () with
  | Error(err) ->
    error "Can't start engine: %s\n" (Error.to_string_hum err);
    Async.exit(1)
  | Ok(p) ->
    print_stderr (Process.stderr p) |> don't_wait_for;
    Monitor.try_with_or_error (fun () -> Deferred.List.iter tests' (f p))
    >>= function
    | Error(err) ->
      error "Unknown error while tests run:\n%s\n" (Error.to_string_hum err);
      Signal.send Signal.term (`Pid (Process.pid p)) |> ignore;
      exit 1
    | Ok(()) ->
      Writer.close (Process.stdin p)
      >>= fun () ->
      Process.wait p
      >>= function
      | Ok(()) -> return ()
      | Error(`Exit_non_zero code) ->
        error "Exit code: %i\n" code;
        exit code
      | _ -> exit 1

let exec_tests prog tests =
  let results = { success = 0; failed = 0 } in
  with_prog prog tests (run_test results) >>| fun () ->
  info "Success: %i; Failed: %i\n" results.success results.failed;
  if results.failed > 0
  then exit 1
  else exit 0

let filter_tests suits =
  if (List.length suits > 0)
  then List.filter Tests.tests (fun (suite, _) ->
      List.mem suits ~equal:String.equal suite)
  else Tests.tests

let exec =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"run tests"
    [%map_open
      let prog = anon ("PROG" %: string)
      and args = flag "--" escape ~doc:"PROG arguments"
      and suits = flag "-s" (listed string) ~doc: "Run only provided test suites"
      and verbose = flag "-verbose" no_arg ~doc:"Verbose logging" in
      fun () ->
        (if verbose then Log.Global.set_level `Debug);
        let output = (Async_extended.Extended_log.Console.output (Lazy.force Writer.stdout)) in
        Log.Global.set_output [output];
        let tests' = filter_tests suits in
        exec_tests (prog, Option.value args ~default:[]) tests' |> ignore;
        Scheduler.go () |> never_returns]

let bench_test n p (e, checks) =
  info "Bench program:\n%s" e;
  let res =
    let open Or_error.Let_syntax in
    let%bind parsed = Orcml.parse e in
    let%bind ir = Orcml.translate_no_deps parsed in
    Orcml.compile ir in
  let fail reason =
    error "Failed with error: %s\n\n" reason in
  match res with
  | Ok((_, repo)) ->
    let input = Process.stdin p in
    let output = Process.stdout p in
    T.Benchmark(Orcml.finalize repo, n) |> Serializer.dump_msg |> Protocol.write input;
    Protocol.read_msg output >>= (function
        | None ->
          error "Engine stopped";
          exit 1
        | Some(Msgpck.Float time) ->
          info "Execution time: %f ms" time;
          return ()
        | Some(v) ->
          error "Bad message %s" (Message_pack.sexp_of_t v |> Sexp.to_string_hum);
          exit 1)
  | Error(err) ->
    fail (Error.to_string_hum err); return ()

let bench_tests prog n tests =
  with_prog prog tests (bench_test n) >>= fun () -> exit 0

let benchmark =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"benchmark orc programs"
    [%map_open
      let prog = anon ("PROG" %: string)
      and n = flag "-n" (optional_with_default 1000 int) ~doc:"Number of iterations"
      and args = flag "--" escape ~doc:"PROG arguments"
      and suits = flag "-s" (listed string) ~doc:"Run only provided test suites" in
      fun () ->
        let tests' = filter_tests suits in
        bench_tests (prog, Option.value args ~default:[]) n tests' |> ignore;
        Scheduler.go () |> never_returns]


let () =
  let output = (Async_extended.Extended_log.Console.output (Lazy.force Writer.stdout)) in
  Log.Global.set_output [output];
  Command.group ~summary:"Tests tool for Orcml"
    [("exec", exec);
     ("benchmark", benchmark)]
  |> Command.run
