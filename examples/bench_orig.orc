def each[A](List[A]) :: A
def each([]) = stop
def each(h:t) = h | each(t)


def map[A,B](lambda (A) :: B, List[A]) :: List[B]
def map(f,[]) = []
def map(f,h:t) = f(h):map(f,t)


def reverse[A](List[A]) :: List[A]
def reverse(l) =
  def tailrev(List[A], List[A]) :: List[A]
  def tailrev([],x) = x
  def tailrev(h:t,x) = tailrev(t,h:x)
  tailrev(l,[])

def filter[A](lambda (A) :: Boolean, List[A]) :: List[A]
def filter(p,[]) = []
def filter(p,x:xs) =
  val fxs = filter(p, xs)
  if p(x) then x:fxs else fxs


def head[A](List[A]) :: A
def head(x:xs) = x


def tail[A](List[A]) :: List[A]
def tail(x:xs) = xs


def init[A](List[A]) :: List[A]
def init([x]) = []
def init(x:xs) = x:init(xs)


def last[A](List[A]) :: A
def last([x]) = x
def last(x:xs) = last(xs)


def empty[A](List[A]) :: Boolean
def empty([]) = true
def empty(_) = false


def index[A](List[A], Integer) :: A
def index(h:t, 0) = h
def index(h:t, n) = index(t, n-1)


def append[A](List[A], List[A]) :: List[A]
def append([],l) = l
def append(h:t,l) = h:append(t,l)


def foldl[A,B](lambda (B, A) :: B, B, List[A]) :: B
def foldl(f,z,[]) = z
def foldl(f,z,x:xs) = foldl(f,f(z,x),xs)


def foldl1[A](lambda (A, A) :: A, List[A]) :: A
def foldl1(f,x:xs) = foldl(f,x,xs)


def foldr[A,B](lambda (A, B) :: B, B, List[A]) :: B
def foldr(f,z,xs) = foldl(flip(f),z,reverse(xs))


def foldr1[A](lambda (A, A) :: A, List[A]) :: A
def foldr1(f,xs) = foldl1(flip(f),reverse(xs))


def afold[A](lambda (A, A) :: A, List[A]) :: A
def afold(f, [x]) = x
{- Here's the interesting part -}
def afold(f, xs) =
  def afold'(List[A]) :: List[A]
  def afold'([]) = []
  def afold'([x]) = [x]
  def afold'(x:y:xs) = f(x,y):afold'(xs)
  afold(f, afold'(xs))


-- def cfold[A](lambda (A, A) :: A, List[A]) :: A
-- def cfold(f, []) = stop
-- def cfold(f, [x]) = x
-- def cfold(f, [x,y]) = f(x,y)
-- def cfold(f, L) =
--   val c = Channel[A]()
--   def work(Number, List[A]) :: A
--   def work(i, x:y:rest) =
--     c.put(f(x,y)) >> stop | work(i+1, rest)
--   def work(i, [x]) = c.put(x) >> stop | work(i+1, [])
--   def work(i, []) =
--     if (i <: 2) then c.get()
--     else c.get() >x> c.get() >y>
--        ( c.put(f(x,y)) >> stop | work(i-1,[]) )
--   work(0, L)


def zipWith[A, B, C](lambda (A, B) :: C, List[A], List[B]) :: List[C]
def zipWith(_, [], _) = []
def zipWith(_, _, []) = []
def zipWith(f, x:xs, y:ys) = f(x, y) : zipWith(f, xs, ys)


def zip[A,B](List[A], List[B]) :: List[(A,B)]
def zip(xs, ys) = zipWith(lambda(x :: A, y :: B) = (x, y), xs, ys)


def unzip[A,B](List[(A,B)]) :: (List[A], List[B])
def unzip([]) = ([],[])
def unzip((x,y):z) = (x:xs,y:ys) <(xs,ys)< unzip(z)


def concat[A](List[List[A]]) :: List[A]
def concat([]) = []
def concat(h:t) = append(h,concat(t))


def length[A](List[A]) :: Integer
def length([]) = 0
def length(h:t) = 1 + length(t)


def take[A](Integer, List[A]) :: List[A]
def take(0, _) = []
def take(n, x:xs) =
  if n :> 0 then x:take(n-1, xs)
  else Error("Cannot take(" + n + ", _)")


def drop[A](Integer, List[A]) :: List[A]
def drop(0, xs) = xs
def drop(n, x:xs) =
  if n :> 0 then drop(n-1, xs)
  else Error("Cannot drop(" + n + ", _)")


def member[A](A, List[A]) :: Boolean
def member(item, []) = false
def member(item, h:t) =
  if item = h then true
  else member(item, t)


def merge[A](List[A], List[A]) :: List[A]
def merge(xs,ys) = mergeBy((<:), xs, ys)


def mergeBy[A](lambda (A,A) :: Boolean,
               List[A], List[A]) :: List[A]
def mergeBy(lt, xs, []) = xs
def mergeBy(lt, [], ys) = ys
def mergeBy(lt, x:xs, y:ys) =
  if lt(y,x) then y:mergeBy(lt,x:xs,ys)
  else x:mergeBy(lt,xs,y:ys)


def sort[A](List[A]) :: List[A]
def sort(xs) = sortBy((<:), xs)


def sortBy[A](lambda (A,A) :: Boolean, List[A]) :: List[A]
def sortBy(lt, []) = []
def sortBy(lt, [x]) = [x]
def sortBy(lt, xs) = xs >> (
  val half = floor(length(xs)/2)
  val front = take(half, xs)
  val back = drop(half, xs)
  mergeBy(lt, sortBy(lt, front), sortBy(lt, back)))


def mergeUnique[A](List[A], List[A]) :: List[A]
def mergeUnique(xs,ys) = mergeUniqueBy((=), (<:), xs, ys)


def mergeUniqueBy[A](lambda (A,A) :: Boolean,
                     lambda (A,A) :: Boolean,
                      List[A], List[A])
  :: List[A]
def mergeUniqueBy(eq, lt, xs, []) = xs
def mergeUniqueBy(eq, lt, [], ys) = ys
def mergeUniqueBy(eq, lt, x:xs, y:ys) =
  if eq(y,x) then mergeUniqueBy(eq, lt, xs, y:ys)
  else if lt(y,x) then y:mergeUniqueBy(eq,lt,x:xs,ys)
  else x:mergeUniqueBy(eq,lt,xs,y:ys)


def sortUnique[A](List[A]) :: List[A]
def sortUnique(xs) = sortUniqueBy((=), (<:), xs)


def sortUniqueBy[A](lambda (A,A) :: Boolean,
                    lambda (A,A) :: Boolean,
                    List[A])
  :: List[A]
def sortUniqueBy(eq, lt, []) = []
def sortUniqueBy(eq, lt, [x]) = [x]
def sortUniqueBy(eq, lt, xs) = xs >> (
  val half = floor(length(xs)/2)
  val front = take(half, xs)
  val back = drop(half, xs)
  mergeUniqueBy(eq, lt,
    sortUniqueBy(eq, lt, front),
    sortUniqueBy(eq, lt, back)))


def group[A,B](List[(A,B)]) :: List[(A,List[B])]
def group(xs) = groupBy((=), xs)


def groupBy[A,B](lambda (A,A) :: Boolean,
                 List[(A,B)])
  :: List[(A,List[B])]
def groupBy(eq, []) = []
def groupBy(eq, (k,v):kvs) =
  def helper(A, List[B], List[(A,B)]) :: List[(A,List[B])]
  def helper(k,vs, []) = [(k,vs)]
  def helper(k,vs, (k2,v):kvs) =
    if eq(k2,k) then helper(k, v:vs, kvs)
    else (k,vs):helper(k2, [v], kvs)
  helper(k,[v], kvs)


def rangeBy(Number, Number, Number) :: List[Number]
def rangeBy(low, high, skip) =
  if low <: high
  then low:rangeBy(low+skip, high, skip)
  else []


def range(Number, Number) :: List[Number]
def range(low, high) = rangeBy(low, high, 1)


def any[A](lambda (A) :: Boolean, List[A]) :: Boolean
def any(p, []) = false
def any(p, x:xs) =
  Let(
    val b1 = p(x)
    val b2 = any(p, xs)
    Ift(b1) >> true | Ift(b2) >> true | (b1 || b2)
  )


def all[A](lambda (A) :: Boolean, List[A]) :: Boolean
def all(p, []) = true
def all(p, x:xs) =
  Let(
    val b1 = p(x)
    val b2 = all(p, xs)
    Iff(b1) >> false | Iff(b2) >> false | (b1 && b2)
  )


def sum(List[Number]) :: Number
def sum(xs) = foldl(
  (+) :: lambda (Number, Number) :: Number,
  0, xs)


def product(List[Number]) :: Number
def product(xs) = foldl(
  (*) :: lambda (Number, Number) :: Number,
  1, xs)


def and(List[Boolean]) :: Boolean
def and([]) = true
def and(false:xs) = false
def and(true:xs) = and(xs)


def or(List[Boolean]) :: Boolean
def or([]) = false
def or(true:xs) = true
def or(false:xs) = or(xs)


def minimum[A](List[A]) :: A
def minimum(xs) =
  def minA(x :: A, y :: A) = min(x,y)
  foldl1(minA, xs)


def maximum[A](List[A]) :: A
def maximum(xs) =
  def maxA(x :: A, y :: A) = max(x,y)
  foldl1(maxA, xs)

1