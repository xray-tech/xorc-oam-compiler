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

let compile_namespaces (module NSLoader : NSLoader) init_queue =
  let compiled = ref [] in
  let mapper ns f =
    let rec acc_rest = function
      | [] -> 0
      | (_, fs, _)::rest -> List.length fs + acc_rest rest in
    let rec calculate = function
      | [] -> raise Orcml.Util.TODO
      | (ns', fs, _)::rest when String.equal ns' ns ->
        let ns_pos = list_position fs ~f:(fun x -> String.equal f x)
                     |> Option.value_exn ~error:(Error.of_exn Orcml.Util.TODO) in
        ns_pos + (acc_rest rest)
      | _::rest -> calculate rest in
    calculate !compiled in
  let module A = Orcml.Ast in
  let fold_decls decl e =
    (A.EDecl(decl, e), A.dummy) in
  let is_already_compiled ns =
    List.find !compiled (fun (ns',_,_) -> String.equal ns ns') |> Option.is_some in
  let analyze code =
    let open Result.Let_syntax in
    let%bind parsed = Orcml_syntax.Syntax.ns_from_string code in
    debug "Parsed NS:\n%s" ([%sexp_of: A.decl list] parsed |> Sexp.to_string_hum);
    let with_stub = List.fold_right parsed ~init:(A.EStop, A.dummy) ~f:fold_decls in
    Orcml.Ir1.translate with_stub in
  let rec compile_loop = function
    | [] ->
      Ok() |> return
    | ns::queue when is_already_compiled ns ->
      Ok() |> return
    | ns::queue ->
      match%bind NSLoader.load ns with
      | None -> Error(`Loader ns) |> return
      | Some(code) ->
        analyze code |> function
        | Error(err) -> Error(`Compiler err) |> return
        | Ok((deps, e)) ->
          compile_loop deps >>=? fun () ->
          match Orcml.Compiler.global_compile_namespace mapper e with
          | Error(err) -> Error(`Compiler err) |> return
          | Ok((fs, bc)) ->
            compiled := (ns, fs, bc)::!compiled;
            Ok() |> return in
  compile_loop init_queue >>|? (fun () ->
      let deps =
        Sequence.of_list !compiled
        |> Sequence.concat_map ~f:(fun (_, _, code) -> Array.to_sequence code)
        |> Sequence.to_array in
      (deps, mapper))

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

let compile_input includes input =
  let loader = multiloader (List.map includes fs) in
  let%bind prog = read_file_or_stdin input in
  let analyzed =
    let open Result.Let_syntax in
    let%bind parsed = Orcml_syntax.Syntax.from_string prog in
    debug "Parsed:\n%s" (Orcml.Ast.sexp_of_e parsed |> Sexp.to_string_hum);
    Orcml.Ir1.translate parsed in
  match analyzed with
  | Error(err) -> Error(err) |> return
  | Ok((deps, ir)) ->
    debug "Translated:\n%s" (Orcml.Ir1.sexp_of_e ir |> Sexp.to_string_hum);
    compile_namespaces loader deps |> Deferred.Result.map_error ~f:(fun err ->
        Error.create_s ([%sexp_of: [> `Compiler of Base.Error.t | `Loader of Core.String.t ]] err))
    >>=? fun (deps, mapper) ->
    match Orcml.Compiler.global_compile mapper ir with
    | Error(_) as err -> return err
    | Ok(compiled) -> Ok (deps, compiled) |> return

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
      and includes = flag "-i" (listed file) ~doc:"Directories to include"
      and verbose = verbose_flag in
      let exec () =
        compile_input includes input >>= fun res ->
        (match res with
         | Error(err) ->
           error "Can't compile: %s" (Error.to_string_hum err);
           exit 1
         | Ok((deps, compiled)) ->
           debug "Compiled:\n%s\nDeps:\n%s"
             (Orcml.Inter.sexp_of_code compiled |> Sexp.to_string_hum)
             (Orcml.Inter.sexp_of_code deps |> Sexp.to_string_hum);
           (match Orcml.Inter.run deps compiled with
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
      handle_res (Some code') (Orcml.Inter.run [||] code')
    | M.Int 1 when Option.is_empty code ->
      error "Unblock before code";
      exit 1
    | M.Int 1 ->
      (Protocol.read_msg_or_exit ~code:0 input >>= function
        | M.List ((M.Int id)::vs) ->
          let (v, _) = Orcml.Serialize.deserialize_value (fun _ -> assert false) vs in
          let res = Orcml.Inter.unblock [||] (Option.value_exn code) (Option.value_exn state) id v in
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
    [(* ("compile", compile); *)
      ("exec", exec);
      ("tests-server", tests_server)]
  |> Command.run
