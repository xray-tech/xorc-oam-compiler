-- returns true 9 times out of 10
def still_alive() = URandom()>u> (u,0.9,u<:0.9)

-- 3.5s avg think time
def think_time() = 1000+Random(6000)

def do_tailmult() =
   def tailmult(a,b) =
     def tailm(0,acc) = acc
     def tailm(a,acc) = tailm(a-1,acc+b)
     tailm(a,0)
   val n = 3+Random(4)
   if      n=3 then  tailmult(2, 30003) | tailmult(20002, 4) | tailmult(10000,123)
   else if n=4 then  tailmult(2, 30003) | tailmult(20002, 4) | tailmult(10000,123)
                     | tailmult(10000,1234)
   else if n=5 then  tailmult(22, 30003) | tailmult(20002, 4) | tailmult(10000,123)
                     | tailmult(10000,1234) | tailmult(16000,1234)
   else              tailmult(2, 3003) | tailmult(20002, 4) | tailmult(10000,123)
                     | tailmult(1200,1234) | tailmult(18000,1234) | tailmult(10000,1234)

def do_8queens() =
    def check((a,b),(x,y)) = Ift(a /= x) >> Ift(b /= y) >> Ift(a - b /= x - y) >> Ift(a + b /= x + y)
    def addqueen(r, []) = [r]
    def addqueen(r, q:qs) = check(r,q) >> q:(addqueen(r,qs))
    def queens(N) =
      def extend(x,0) = x
      def extend(x,n) = extend(x,n-1) >y> upto(N) >j> addqueen((n,j), y)
      extend([],N)
   defer(queens,6)()


def do_http() =
   val x = Random(8)
   val url = (
    if x=0 then "https://httpbin.org/get?show_env=1"
    else if x=1 then "https://httpbin.org/delay/2?show_env=1"
    else if x=2 then "https://avwx.aeronavmap.com/metar/EDDT"
    else if x=3 then "https://avwx.aeronavmap.com/metar/EDDT?options=speech,info,translate,summary"
    else if x=4 then "https://www.quandl.com/api/v3/datasets/WIKI/FB/data.json?api_key=m1zpbJJGLv3TT-xAkXGm"
    else if x=5 then "https://www.quandl.com/api/v3/datasets/WIKI/AAPL.json?api_key=m1zpbJJGLv3TT-xAkXGm"
    else if x=6 then "https://edition.cnn.com"
    else "https://httpbin.org/status/200")
   HTTP(url).get()>json>(if x/=7 && x/= 6 then ReadJSON(json) else signal)


def shuffle(a,b) = if (Random(2) = 1) then (a,b) else (b,a)

def take((a,b)) =    
  a.acquire() >> b.acquireD() ;
  a.release() >> take(shuffle(a,b))
    
def drop(a,b) = (a.release(), b.release()) >> signal


def phil(n,a,b) =
  def think() = 
    Println("thinking: "+n) >> 
    (if still_alive())
      then
        val w = think_time()
        Println("philosopher "+n+" thinking for "+w+"ms ">>
        Rwait(w) -- 3.5 seconds average
      else Println("philosopher "+n+": died of exhaustion.")
  def can_work() = take((a,b))
  def work() =
    val clock = Rclock().time
    Random(3) >r>
    (if      r=0 then {. f=do_tailmult, s="tailmult" .}
     else if r=1 then {. f=do_8queens, s="8queens" .}
     else             {. f=do_http, s="http" .} )  >doit>
    collect(doit.f)>res>
    Println("philosopher " + n + " done: task="+doit.s+" time=" + clock()) >> 
    drop(a,b)
  think() >> can_work() >> work() >> phil(n,a,b)

def philosophers(1,a,b) = phil(1,a,b)
def philosophers(n,a,b) =
  val c = Semaphore(1)
  philosophers(n-1,a,c) | phil(n,c,b)

val fork = Semaphore(1)
philosophers(5,fork,fork)
