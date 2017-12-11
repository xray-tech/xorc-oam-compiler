open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.simple
    (module Main)
    ~initial_model:(Main.Model.Fields.create ~program:"" ~values:[])
