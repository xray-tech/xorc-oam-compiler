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

let read_file_or_stdin = function
  | None -> Reader.contents (Lazy.force Reader.stdin)
  | Some(f) -> Reader.file_contents f

module type NSLoader = sig
  val load : string -> string option Deferred.t
end

let list_position l ~f =
  let rec step acc = function
    | [] -> None
    | x::xs when f x -> Some(acc)
    | x::xs -> step (acc + 1) xs in
  step 0 l

let imports_linker imports_mapping fun_ =
  let equal a b = Int.equal 0 (Orcml.Fun.compare a b) in
  (match List.Assoc.find imports_mapping ~equal fun_  with
   | Some(pointer) -> Ok(pointer)
   | None -> Or_error.error_s ([%sexp_of: string * Sexp.t] ("Unbound dependency", Orcml.Fun.sexp_of_t fun_)))

let link_imports imports_mapping imports repo =
  let linker p = match List.Assoc.find imports ~equal:Int.equal p with
    | None -> Ok(p)
    | Some(x) ->
      imports_linker imports_mapping x in
  Orcml.link repo linker

let link_namespaces compiled =
  with_return (fun r ->
      let change_pointers shift imports_mapping imports repo =
        let size = List.length repo in
        let linker p = match List.Assoc.find imports ~equal:Int.equal p with
          (* it's local fun, so we add shift to it and do not forget that result
             code is reversed *)
          | None ->
            Ok(shift + (size - (Int.abs p)))
          (* it's pointer to external fun, we should already have it *)
          | Some(x) -> imports_linker imports_mapping x in
        Orcml.link repo linker in
      let fold_compiled (imports_mapping, all_code) (ns, (imports, repo)) =
        let shift = List.length all_code in
        match change_pointers shift imports_mapping imports repo with
        | Error(err) -> r.return (Error err)
        | Ok(repo') ->
          let m = List.mapi (List.rev repo) (fun i (name, _) -> (Orcml.Fun.{ns; name}, shift + i)) in
          (m @ imports_mapping, repo' @ all_code) in
      let (global_mapping, code) =
        List.fold (List.rev compiled) ~init:([], []) ~f:fold_compiled in
      Ok(Orcml.finalize (List.rev code), global_mapping))

let compile_namespaces (module NSLoader : NSLoader) init_queue =
  let compiled = ref [] in
  let is_already_compiled ns =
    List.find !compiled (fun (ns', _) -> String.equal ns ns') |> Option.is_some in
  let analyze code =
    let open Result.Let_syntax in
    let%bind parsed = Orcml.parse_ns code in
    debug "Parsed NS:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum);
    Orcml.translate parsed in
  let rec compile_loop = function
    | [] ->
      Ok() |> return
    | ns::queue when is_already_compiled ns ->
      compile_loop queue
    | ns::queue ->
      match%bind NSLoader.load ns with
      | None -> Error(Error.createf "Can't load %s" ns) |> return
      | Some(code) ->
        analyze code |> function
        | Error(err) -> Error(err) |> return
        | Ok((deps, e)) ->
          compile_loop deps >>=? fun () ->
          match Orcml.compile_ns e with
          | Error(err) -> Error(err) |> return
          | Ok(res) ->
            compiled := (ns, res)::!compiled;
            compile_loop queue in
  compile_loop init_queue >>=? (fun () ->
      return (link_namespaces !compiled))

let optional_file_contents path =
  Monitor.try_with (fun () -> Reader.file_contents path >>| Option.some )
  >>| function
  | Ok(v) -> v
  | Error(_) -> None

let fs path =
  (module struct
    let load ns =
      let ns' = String.Search_pattern.(replace_all (create "\.") ~in_:ns ~with_:"/") ^ ".orc" in
      optional_file_contents (Filename.concat path ns')
  end : NSLoader)

let multiloader loaders =
  (module struct
    let load ns = Deferred.List.find_map loaders (fun (module Loader : NSLoader) ->
        Loader.load ns)
  end : NSLoader)

let empty_loader =
  (module struct
    let load ns = return None
  end : NSLoader)

let implicit_prelude =
  "refer from core (abs, signum, min, max)
   refer from idioms (curry, curry3, uncurry, uncurry3, flip, constant, defer, defer2,
     ignore, ignore2, compose, while, repeat, fork, forkMap, seq, seqMap, join,
     joinMap, alt, altMap, por, pand)
   refer from list (each, map, reverse, filter, head, tail, init, last, empty, index,
     append, foldl, foldl1, foldr, foldr1, afold, zipWith, zip, unzip, concat,
     length, take, drop, member, merge, mergeBy, sort, sortBy, mergeUnique,
     mergeUniqueBy, sortUnique, sortUniqueBy, group, groupBy, rangeBy, range,
     any, all, sum, product, and, or, minimum, maximum) "

let compile_input prelude includes input =
  let loader = multiloader (List.map includes fs) in
  let%bind prog = read_file_or_stdin input in
  let prog' = if prelude
    then implicit_prelude ^ prog
    else prog in
  let analyzed =
    let open Result.Let_syntax in
    let%bind parsed = Orcml.parse prog' in
    debug "Parsed:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum);
    Orcml.translate parsed in
  match analyzed with
  | Error(err) -> Error(err) |> return
  | Ok((deps, ir)) ->
    debug "Translated:\n%s" (Orcml.sexp_of_ir1 ir |> Sexp.to_string_hum);
    compile_namespaces loader deps
    >>=? fun (deps, imports_mapping) ->
    match Result.(
        Orcml.compile ir >>= (fun (imports, repo) ->
            debug "Before linking:\n%s" (Orcml.sexp_of_repo repo |> Sexp.to_string_hum);
            link_imports imports_mapping imports repo)) with
    | Error(err) -> Error err |> return
    | Ok(repo') -> Ok(deps, Orcml.finalize repo') |> return

(* let compile =
 *   let open Command.Let_syntax in
 *   Command.basic'
 *     ~summary: "produce bytecode"
 *     [%map_open
 *       let input = anon (maybe ("INPUT" %: file))
 *       and bc = flag "-bc" no_arg ~doc:"Execute bytecode, not Orc source file. By default reads from stdin"
 *       and verbose = verbose_flag in
 *       let exec () =
 *         compile_input includes input >>= fun res ->
 *         (match res with
 *          | Error(err) ->
 *            error "Can't compile: %s" (Error.to_string_hum err);
 *            exit 1
 *          | Ok(compiled) ->
 *            Orcml.Serialize.serialize_bc compiled |> print_string;
 *            exit 0) in
 *       fun () ->
 *         set_logging verbose;
 *         exec () |> ignore;
 *         Scheduler.go () |> never_returns] *)

let generate_result state_path {Orcml.Res.values; coeffects; instance} =
  List.iter values (fun v ->
      info "Value: %s" (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum));
  List.iter coeffects (fun (id, v) ->
      info "Coeffect: %i -> %s" id (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum));
  exit 0

let exec =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "executes orc"
    [%map_open
      let input = anon (maybe ("INPUT" %: file))
      and bc = flag "-bc" no_arg ~doc:"Execute bytecode, not Orc source file. By default reads from stdin"
      and prelude = flag "-prelude" no_arg ~doc:"Implicity refer whole prelude"
      and state = flag "-state" (optional file) ~doc:"Path to store intermediate state if any"
      and includes = flag "-i" (listed file) ~doc:"Directories to include"
      and verbose = verbose_flag in
      let exec () =
        compile_input prelude includes input >>= fun res ->
        (match res with
         | Error(err) ->
           error "Can't compile: %s" (Error.to_string_hum err);
           exit 1
         | Ok((dependencies, compiled)) ->
           debug "Compiled:\n%s\nDeps:\n%s"
             (Orcml.sexp_of_bc compiled |> Sexp.to_string_hum)
             (Orcml.sexp_of_bc dependencies |> Sexp.to_string_hum);
           (match Orcml.run ~dependencies compiled with
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
  let module Serializer = Orcml.Testkit.Serializer in
  let module Protocol = Orcml_testkit.Protocol in
  let write v = Protocol.write (Lazy.force Writer.stdout) v in
  let write_result r =
    write (Serializer.dump_res r) in
  let rec server code state =
    let input = (Lazy.force Reader.stdin) in
    Protocol.read_msg_or_exit ~code:0 input >>= fun msg ->
    match Serializer.load_msg msg |> Or_error.ok_exn with
    | Execute(bc) ->
      handle_res (Some bc) (Orcml.run bc)
    | Continue(id, v) ->
      let res = Orcml.unblock (Option.value_exn code) (Option.value_exn state) id v in
      handle_res code res
    | Benchmark(bc, iter) ->
      (match Benchmark.latency1 ~style:Benchmark.Nil
               (Int64.of_int iter) Orcml.run bc with
      | [(_, [{Benchmark.wall}])] ->
        write (Serializer.dump_bench_res (wall *. 1000.0));
        server None None
      | _ -> assert false)
  and handle_res code = function
    | Ok(({Orcml.Res.instance} as v)) ->
      write_result v;
      server code (Some instance)
    | Error(err) ->
      error "Runtime error:\n%s" (Error.to_string_hum err);
      exit 1 in
  let open Command.Let_syntax in
  Command.basic
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
    [(* ("compile", compile); *)
      ("exec", exec);
      ("tests-server", tests_server)]
  |> Command.run
