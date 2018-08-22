def check((a,b),(x,y)) = Ift(a /= x) >> Ift(b /= y) >> Ift(a - b /= x - y) >> Ift(a + b /= x + y)

def addqueen(r, []) = [r]
def addqueen(r, q:qs) = check(r,q) >> q:(addqueen(r,qs))

def queens(N) =
  def extend(x,0) = x
  def extend(x,n) = extend(x,n-1) >y> upto(N) >j> addqueen((n,j), y)
  extend([],N)

val clock = Rclock().time
collect(defer(queens, 6)) >x>
Println("Time elapsed: " + clock()) >>
each(x)
