def foo(x) =
  if x = 0 then "ok" else foo(x - 1)

foo(100000)