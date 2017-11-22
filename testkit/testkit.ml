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

let run_test p results (e, checks) =
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

let print_stderr p =
  Reader.contents (Process.stderr p)
  >>| (fun v -> error "Engine stderr:\n%s\n\n" v)

let run_tests (prog, args) tests =
  let results = { success = 0; failed = 0 } in
  let tests' = List.concat_map tests (fun (_, l) -> l) in
  let open Async.Let_syntax in
  match%bind Process.create prog args () with
  | Error(err) ->
    error "Can't start engine: %s\n" (Error.to_string_hum err);
    Async.exit(1)
  | Ok(p) ->
    Monitor.try_with_or_error (fun () -> Deferred.List.iter tests' (run_test p results))
    >>= function
    | Error(err) ->
      error "Unknown error while tests run:\n%s\n" (Error.to_string_hum err);
      print_stderr p >>= fun () ->
      exit 1
    | Ok(()) ->
      info "Success: %i; Failed: %i\n" results.success results.failed;
      Writer.close (Process.stdin p)
      >>= fun () ->
      Process.wait p
      >>= function
      | Ok(()) -> exit 0
      | Error(`Exit_non_zero code) ->
        error "Exit code: %i\n" code;
        print_stderr p >>= fun () ->
        exit code
      | _ -> exit 1

let () =
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
        let tests' = if (List.length suits > 0)
          then List.filter Tests.tests (fun (suite, _) ->
              List.mem suits ~equal:String.equal suite)
          else Tests.tests in
        run_tests (prog, Option.value args ~default:[]) tests' |> ignore;
        Scheduler.go () |> never_returns]
  |> Command.run
