def curry(f) = lambda(x) = lambda(y) = f(x,y)

def curry3(f) = lambda(x) = lambda(y) = lambda(z) = f(x,y,z)

def uncurry(f) = lambda(x,y) = f(x)(y)

def uncurry3(f) = lambda(x,y,z) = f(x)(y)(z)

def flip(f) = lambda(x, y) = f(y,x)

def constant(x) = lambda() = x

def defer(f, x) = lambda() = f(x)

def defer2(f, x, y) = lambda() = f(x, y)

def ignore(f) = lambda(_) = f()

def ignore2(f) = lambda(_, _) = f()

def compose(f,g) = lambda(x) = f(g(x))

def while(p,f) =
  def loop(x) = Ift(p(x)) >> ( x | loop(f(x)) )
  loop

def repeat(f) = f() >x> (x | repeat(f))

def fork([]) = stop
def fork(p:ps) = p() | fork(ps)

def forkMap(f, []) = stop
def forkMap(f, x:xs) = f(x) | forkMap(f, xs)

def seq([]) = signal
def seq(p:ps) = p() >> seq(ps)

def seqMap(f, []) = signal
def seqMap(f, x:xs) = f(x) >> seqMap(f, xs)

def join([]) = signal
def join(p:ps) = (p(), join(ps)) >> signal

def joinMap(f, []) = signal
def joinMap(f, x:xs) = (f(x), joinMap(f, xs)) >> signal

def alt([]) = stop
def alt(p:ps) = p() ; alt(ps)

def altMap(f, []) = stop
def altMap(f, x:xs) = f(x) ; altMap(f, xs)

def por([]) = false
def por(p:ps) =
  Let(
    val b1 = p()
    val b2 = por(ps)
    Ift(b1) >> true | Ift(b2) >> true | (b1 || b2)
  )

def pand([]) = true
def pand(p:ps) =
  Let(
    val b1 = p()
    val b2 = pand(ps)
    Iff(b1) >> false | Iff(b2) >> false | (b1 && b2)
  )


-- def collect[A](lambda () :: A) :: List[A]
-- def collect(p) =
--   val b = Channel[A]()
--   p() >x> b.put(x) >> stop
--   ; b.getAll()