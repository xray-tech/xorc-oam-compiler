open! Core
open! Async
open! Log.Global

module Lexer = Orcml_lexer
open Orcml_testkit

type results = { mutable success : int;
                 mutable failed : int }

let strings_to_values vs =
  List.map vs Orcml_syntax.Syntax.value
  |> List.map ~f:(fun v -> Option.value_exn v)

let values_to_set = Set.of_list ~comparator:Orcml.Inter.Value.comparator

let print_stderr p =
  Reader.contents (Process.stderr p)
  >>| (fun v -> error "Engine stderr:\n%s\n\n" v)

let run_loop p fail compiled checks =
  let input = Process.stdin p in
  let output = Process.stdout p in
  Protocol.write_code input compiled;

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
                 ([%sexp_of: Orcml.Inter.v list] expected' |> Sexp.to_string_hum)
                 ([%sexp_of: Orcml.Inter.v list] actual |> Sexp.to_string_hum))
    | OneOf expected ->
      let expected' = strings_to_values expected in
      let expected'' = values_to_set expected' in
      (match actual with
       | [v] ->
         if (Set.mem expected'' v) then
           `Ok
         else
           `Fail (sprintf "expected one of: %s\nactual: %s"
                    ([%sexp_of: Orcml.Inter.v list] expected' |> Sexp.to_string_hum)
                    ([%sexp_of: Orcml.Inter.v] v |> Sexp.to_string_hum))
       | vals ->
         `Fail (sprintf "expected only one of %s\nactual: %s"
                  ([%sexp_of: Orcml.Inter.v list] expected' |> Sexp.to_string_hum)
                  ([%sexp_of: Orcml.Inter.v list] vals |> Sexp.to_string_hum))) in
  let rec tick check =
    Protocol.read_res output >>= function
    | None ->
      error "Engine stopped";
      print_stderr p >>= fun () ->
      exit 1
    | Some((actual, killed)) ->
      match check with
      | Tests.Check expected ->
        check_values expected actual |> return
      | Tests.CheckAndResume { values; unblock = (id, v); next } ->
        (match check_values values actual with
         | `Ok ->
           let v = Orcml_syntax.Syntax.value v |> Option.value_exn in
           Protocol.write_unblock input id v;
           tick next
         | other -> return other)
  in
  tick checks

let run_test results p (e, checks) =
  debug "Run test: %s" e;
  let res = Orcml_syntax.Syntax.from_string e
            |> Result.bind ~f:Orcml.Ir1.translate
            |> Result.bind ~f:Orcml.Compiler.compile in
  let fail reason =
    results.failed <- results.failed + 1;
    error "Test program:\n%s\nFailed with error: %s\n\n" e reason in
  match res with
  | Ok(compiled) ->
    run_loop p fail compiled checks >>| (function
        | `Ok -> results.success <- results.success + 1
        | `Fail reason ->
          fail reason)
  | Error(err) ->
    fail (Orcml.Errors.format err); return ()

let with_prog (prog, args) tests f =
  let tests' = List.concat_map tests (fun (_, l) -> l) in
  let open Async.Let_syntax in
  match%bind Process.create prog args () with
  | Error(err) ->
    error "Can't start engine: %s\n" (Error.to_string_hum err);
    Async.exit(1)
  | Ok(p) ->
    Monitor.try_with_or_error (fun () -> Deferred.List.iter tests' (f p))
    >>= function
    | Error(err) ->
      error "Unknown error while tests run:\n%s\n" (Error.to_string_hum err);
      Signal.send Signal.term (`Pid (Process.pid p)) |> ignore;
      print_stderr p >>= fun () ->
      exit 1
    | Ok(()) ->
      Writer.close (Process.stdin p)
      >>= fun () ->
      (* print_stderr p >>= fun () -> *)
      Process.wait p
      >>= function
      | Ok(()) -> return ()
      | Error(`Exit_non_zero code) ->
        error "Exit code: %i\n" code;
        print_stderr p >>= fun () ->
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
  let res = Orcml_syntax.Syntax.from_string e
            |> Result.bind ~f:Orcml.Ir1.translate
            |> Result.bind ~f:Orcml.Compiler.compile in
  let fail reason =
    error "Failed with error: %s\n\n" reason in
  match res with
  | Ok(compiled) ->
    let input = Process.stdin p in
    let output = Process.stdout p in
    Protocol.write_bench input compiled n;
    Protocol.read_msg output >>= (function
        | None ->
          error "Engine stopped";
          print_stderr p >>= fun () -> exit 1
        | Some(Msgpck.Float time) ->
          info "Execution time: %f ms" time;
          return ()
        | Some(v) ->
          error "Bad message %s" (Message_pack.sexp_of_t v |> Sexp.to_string_hum);
          print_stderr p >>= fun () -> exit 1)
  | Error(err) ->
    fail (Orcml.Errors.format err); return ()

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
