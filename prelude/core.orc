def abs(x) = if x <: 0 then -x else x

def signum(x) =
  if x <: 0 then -1
  else if x :> 0 then 1
  else 0

def min(x,y) = if y <: x then y else x

def max(x,y) = if x :> y then x else y

def eq(x,y) = (x = y)