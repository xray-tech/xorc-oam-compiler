sig each[A](List[A]) :: A
def each([]) = stop
def each(h:t) = h | each(t)


sig map[A,B](lambda (A) :: B, List[A]) :: List[B]
def map(f,[]) = []
def map(f,h:t) = f(h):map(f,t)


sig reverse[A](List[A]) :: List[A]
def reverse(l) =
  def tailrev(List[A], List[A]) :: List[A]
  def tailrev([],x) = x
  def tailrev(h:t,x) = tailrev(t,h:x)
  tailrev(l,[])

sig filter[A](lambda (A) :: Boolean, List[A]) :: List[A]
def filter(p,[]) = []
def filter(p,x:xs) =
  val fxs = filter(p, xs)
  if p(x) then x:fxs else fxs


sig head[A](List[A]) :: A
def head(x:xs) = x


sig tail[A](List[A]) :: List[A]
def tail(x:xs) = xs


sig init[A](List[A]) :: List[A]
def init([x]) = []
def init(x:xs) = x:init(xs)


sig last[A](List[A]) :: A
def last([x]) = x
def last(x:xs) = last(xs)


sig empty[A](List[A]) :: Boolean
def empty([]) = true
def empty(_) = false


sig index[A](List[A], Integer) :: A
def index(h:t, 0) = h
def index(h:t, n) = index(t, n-1)


sig append[A](List[A], List[A]) :: List[A]
def append([],l) = l
def append(h:t,l) = h:append(t,l)


sig foldl[A,B](lambda (B, A) :: B, B, List[A]) :: B
def foldl(f,z,[]) = z
def foldl(f,z,x:xs) = foldl(f,f(z,x),xs)


sig foldl1[A](lambda (A, A) :: A, List[A]) :: A
def foldl1(f,x:xs) = foldl(f,x,xs)


sig foldr[A,B](lambda (A, B) :: B, B, List[A]) :: B
def foldr(f,z,xs) = foldl(flip(f),z,reverse(xs))


sig foldr1[A](lambda (A, A) :: A, List[A]) :: A
def foldr1(f,xs) = foldl1(flip(f),reverse(xs))


sig afold[A](lambda (A, A) :: A, List[A]) :: A
def afold(f, [x]) = x
{- Here's the interesting part -}
def afold(f, xs) =
  def afold'(List[A]) :: List[A]
  def afold'([]) = []
  def afold'([x]) = [x]
  def afold'(x:y:xs) = f(x,y):afold'(xs)
  afold(f, afold'(xs))



sig zipWith[A, B, C](lambda (A, B) :: C, List[A], List[B]) :: List[C]
def zipWith(_, [], _) = []
def zipWith(_, _, []) = []
def zipWith(f, x:xs, y:ys) = f(x, y) : zipWith(f, xs, ys)


sig zip[A,B](List[A], List[B]) :: List[(A,B)]
def zip(xs, ys) = zipWith(lambda(x :: A, y :: B) = (x, y), xs, ys)


sig unzip[A,B](List[(A,B)]) :: (List[A], List[B])
def unzip([]) = ([],[])
def unzip((x,y):z) = (x:xs,y:ys) <(xs,ys)< unzip(z)


sig concat[A](List[List[A]]) :: List[A]
def concat([]) = []
def concat(h:t) = append(h,concat(t))


sig length[A](List[A]) :: Integer
def length([]) = 0
def length(h:t) = 1 + length(t)


sig take[A](Integer, List[A]) :: List[A]
def take(0, _) = []
def take(n, x:xs) =
  if n :> 0 then x:take(n-1, xs)
  else Error("Cannot take(" + n + ", _)")


sig drop[A](Integer, List[A]) :: List[A]
def drop(0, xs) = xs
def drop(n, x:xs) =
  if n :> 0 then drop(n-1, xs)
  else Error("Cannot drop(" + n + ", _)")


sig member[A](A, List[A]) :: Boolean
def member(item, []) = false
def member(item, h:t) =
  if item = h then true
  else member(item, t)


sig merge[A](List[A], List[A]) :: List[A]
def merge(xs,ys) = mergeBy((<:), xs, ys)


sig mergeBy[A](lambda (A,A) :: Boolean,
               List[A], List[A]) :: List[A]
def mergeBy(lt, xs, []) = xs
def mergeBy(lt, [], ys) = ys
def mergeBy(lt, x:xs, y:ys) =
  if lt(y,x) then y:mergeBy(lt,x:xs,ys)
  else x:mergeBy(lt,xs,y:ys)


sign sort[A](List[A]) :: List[A]
def sort(xs) = sortBy((<:), xs)


sig sortBy[A](lambda (A,A) :: Boolean, List[A]) :: List[A]
def sortBy(lt, []) = []
def sortBy(lt, [x]) = [x]
def sortBy(lt, xs) = xs >> (
  val half = floor(length(xs)/2)
  val front = take(half, xs)
  val back = drop(half, xs)
  mergeBy(lt, sortBy(lt, front), sortBy(lt, back)))


sig mergeUnique[A](List[A], List[A]) :: List[A]
def mergeUnique(xs,ys) = mergeUniqueBy((=), (<:), xs, ys)


sig mergeUniqueBy[A](lambda (A,A) :: Boolean,
                     lambda (A,A) :: Boolean,
                      List[A], List[A])
  :: List[A]
def mergeUniqueBy(eq, lt, xs, []) = xs
def mergeUniqueBy(eq, lt, [], ys) = ys
def mergeUniqueBy(eq, lt, x:xs, y:ys) =
  if eq(y,x) then mergeUniqueBy(eq, lt, xs, y:ys)
  else if lt(y,x) then y:mergeUniqueBy(eq,lt,x:xs,ys)
  else x:mergeUniqueBy(eq,lt,xs,y:ys)


sig sortUnique[A](List[A]) :: List[A]
def sortUnique(xs) = sortUniqueBy((=), (<:), xs)


sig sortUniqueBy[A](lambda (A,A) :: Boolean,
                    lambda (A,A) :: Boolean,
                    List[A])
  :: List[A]
sig sortUniqueBy(eq, lt, []) = []
def sortUniqueBy(eq, lt, [x]) = [x]
def sortUniqueBy(eq, lt, xs) = xs >> (
  val half = floor(length(xs)/2)
  val front = take(half, xs)
  val back = drop(half, xs)
  mergeUniqueBy(eq, lt,
    sortUniqueBy(eq, lt, front),
    sortUniqueBy(eq, lt, back)))


sig group[A,B](List[(A,B)]) :: List[(A,List[B])]
def group(xs) = groupBy((=), xs)


sig groupBy[A,B](lambda (A,A) :: Boolean,
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


sig rangeBy(Number, Number, Number) :: List[Number]
def rangeBy(low, high, skip) =
  if low <: high
  then low:rangeBy(low+skip, high, skip)
  else []


sig range(Number, Number) :: List[Number]
def range(low, high) = rangeBy(low, high, 1)


sig any[A](lambda (A) :: Boolean, List[A]) :: Boolean
def any(p, []) = false
def any(p, x:xs) =
  Let(
    val b1 = p(x)
    val b2 = any(p, xs)
    Ift(b1) >> true | Ift(b2) >> true | (b1 || b2)
  )


sig all[A](lambda (A) :: Boolean, List[A]) :: Boolean
def all(p, []) = true
def all(p, x:xs) =
  Let(
    val b1 = p(x)
    val b2 = all(p, xs)
    Iff(b1) >> false | Iff(b2) >> false | (b1 && b2)
  )


sig sum(List[Number]) :: Number
def sum(xs) = foldl(
  (+) :: lambda (Number, Number) :: Number,
  0, xs)


sig product(List[Number]) :: Number
def product(xs) = foldl(
  (*) :: lambda (Number, Number) :: Number,
  1, xs)


sig and(List[Boolean]) :: Boolean
def and([]) = true
def and(false:xs) = false
def and(true:xs) = and(xs)


sig or(List[Boolean]) :: Boolean
def or([]) = false
def or(true:xs) = true
def or(false:xs) = or(xs)


sig minimum[A](List[A]) :: A
def minimum(xs) =
  def minA(x :: A, y :: A) = min(x,y)
  foldl1(minA, xs)


sig maximum[A](List[A]) :: A
def maximum(xs) =
  def maxA(x :: A, y :: A) = max(x,y)
  foldl1(maxA, xs)

1