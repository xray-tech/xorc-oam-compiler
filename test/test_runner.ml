let eq1 () = Alcotest.(check string "same words" "world" Orcml.Mylib.name)

let basic = [
  "Eq1", `Quick, eq1
]

let () =
  Alcotest.run "orcml" [
    "basic", basic;
  ]
