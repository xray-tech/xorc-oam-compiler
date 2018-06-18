sig signals(Integer) :: Signal
def signals(n) = if n :> 0 then (signal | signals(n-1)) else stop

sig for(Integer, Integer) :: Integer
def for(low, high) =
  if low >= high then stop
  else ( low | for(low+1, high) )

sig upto(Integer) :: Integer
def upto(high) = for(0, high)