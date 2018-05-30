refer from idioms (flip)
refer from core (min, max, (+), (-), (:), (<:), (:>), (=), (/), floor, Error)
def each([]) = stop
def each(h:t) = h | each(t)

def map(f,[]) = []
def map(f,h:t) = f(h):map(f,t)

def reverse(l) =
  def tailrev([],x) = x
  def tailrev(h:t,x) = tailrev(t,h:x)
  tailrev(l,[])

def filter(p,[]) = []
def filter(p,x:xs) =
  val fxs = filter(p, xs)
  if p(x) then x:fxs else fxs

def head(x:xs) = x

def tail(x:xs) = xs

def init([x]) = []
def init(x:xs) = x:init(xs)

def last([x]) = x
def last(x:xs) = last(xs)

def empty([]) = true
def empty(_) = false

def index(h:t, 0) = h
def index(h:t, n) = index(t, n-1)

def append([],l) = l
def append(h:t,l) = h:append(t,l)

def foldl(f,z,[]) = z
def foldl(f,z,x:xs) = foldl(f,f(z,x),xs)

def foldl1(f,x:xs) = foldl(f,x,xs)

def foldr(f,z,xs) = foldl(flip(f),z,reverse(xs))

def foldr1(f,xs) = foldl1(flip(f),reverse(xs))

def afold(f, [x]) = x
{- Here's the interesting part -}
def afold(f, xs) =
  def afold'([]) = []
  def afold'([x]) = [x]
  def afold'(x:y:xs) = f(x,y):afold'(xs)
  afold(f, afold'(xs))


sig cfold[A](lambda (A, A) :: A, List[A]) :: A
def cfold(f, []) = stop
def cfold(f, [x]) = x
def cfold(f, [x,y]) = f(x,y)
def cfold(f, L) =
   refer from state (Channel)
   val c = Channel[A]()
   sig work(Number, List[A]) :: A
   def work(i, x:y:rest) =
     c.put(f(x,y)) >> stop | work(i+1, rest)
   def work(i, [x]) = c.put(x) >> stop | work(i+1, [])
   def work(i, []) =
     if (i <: 2) then c.get()
     else c.get() >x> c.get() >y>
        ( c.put(f(x,y)) >> stop | work(i-1,[]) )
   work(0, L)

def zipWith(_, [], _) = []
def zipWith(_, _, []) = []
def zipWith(f, x:xs, y:ys) = f(x, y) : zipWith(f, xs, ys)

def zip(xs, ys) = zipWith(lambda(x, y) = (x, y), xs, ys)

def unzip([]) = ([],[])
def unzip((x,y):z) = (x:xs,y:ys) <(xs,ys)< unzip(z)

def concat([]) = []
def concat(h:t) = append(h,concat(t))

def length([]) = 0
def length(h:t) = 1 + length(t)

def take(0, _) = []
def take(n, x:xs) =
  if n :> 0 then x:take(n-1, xs)
  else Error("Cannot take(" + n + ", _)")

def drop(0, xs) = xs
def drop(n, x:xs) =
  if n :> 0 then drop(n-1, xs)
  else Error("Cannot drop(" + n + ", _)")

def member(item, []) = false
def member(item, h:t) =
  if item = h then true
  else member(item, t)

def merge(xs,ys) = mergeBy((<:), xs, ys)

def mergeBy(lt, xs, []) = xs
def mergeBy(lt, [], ys) = ys
def mergeBy(lt, x:xs, y:ys) =
  if lt(y,x) then y:mergeBy(lt,x:xs,ys)
  else x:mergeBy(lt,xs,y:ys)

def sort(xs) = sortBy((<:), xs)

def sortBy(lt, []) = []
def sortBy(lt, [x]) = [x]
def sortBy(lt, xs) = xs >> (
  val half = floor(length(xs)/2)
  val front = take(half, xs)
  val back = drop(half, xs)
  mergeBy(lt, sortBy(lt, front), sortBy(lt, back)))


def mergeUnique(xs,ys) = mergeUniqueBy((=), (<:), xs, ys)

def mergeUniqueBy(eq, lt, xs, []) = xs
def mergeUniqueBy(eq, lt, [], ys) = ys
def mergeUniqueBy(eq, lt, x:xs, y:ys) =
  if eq(y,x) then mergeUniqueBy(eq, lt, xs, y:ys)
  else if lt(y,x) then y:mergeUniqueBy(eq,lt,x:xs,ys)
  else x:mergeUniqueBy(eq,lt,xs,y:ys)

def sortUnique(xs) = sortUniqueBy((=), (<:), xs)


def sortUniqueBy(eq, lt, []) = []
def sortUniqueBy(eq, lt, [x]) = [x]
def sortUniqueBy(eq, lt, xs) = xs >> (
  val half = floor(length(xs)/2)
  val front = take(half, xs)
  val back = drop(half, xs)
  mergeUniqueBy(eq, lt,
    sortUniqueBy(eq, lt, front),
    sortUniqueBy(eq, lt, back)))

def group(xs) = groupBy((=), xs)

def groupBy(eq, []) = []
def groupBy(eq, (k,v):kvs) =
  def helper(k,vs, []) = [(k,vs)]
  def helper(k,vs, (k2,v):kvs) =
    if eq(k2,k) then helper(k, v:vs, kvs)
    else (k,vs):helper(k2, [v], kvs)
  helper(k,[v], kvs)

def rangeBy(low, high, skip) =
  if low <: high
  then low:rangeBy(low+skip, high, skip)
  else []

def range(low, high) = rangeBy(low, high, 1)

def any(p, []) = false
def any(p, x:xs) =
  Let(
    val b1 = p(x)
    val b2 = any(p, xs)
    Ift(b1) >> true | Ift(b2) >> true | (b1 || b2)
  )

def all(p, []) = true
def all(p, x:xs) =
  Let(
    val b1 = p(x)
    val b2 = all(p, xs)
    Iff(b1) >> false | Iff(b2) >> false | (b1 && b2)
  )

def sum(xs) = foldl((+), 0, xs)

def product(xs) = foldl((*), 1, xs)

def and([]) = true
def and(false:xs) = false
def and(true:xs) = and(xs)

def or([]) = false
def or(true:xs) = true
def or(false:xs) = or(xs)

def minimum(xs) =
  def minA(x, y) = min(x,y)
  foldl1(minA, xs)

def maximum(xs) =
  def maxA(x, y) = max(x,y)
  foldl1(maxA, xs)
