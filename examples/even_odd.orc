def even(x) = if x = 0 then true else odd(x - 1)
def odd(x) = if x = 0 then false else even(x - 1)

even(5)