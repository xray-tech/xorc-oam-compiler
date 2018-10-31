-- 
-- def trace(x)=Println(x)
def trace(x)=Coeffect({. name="trace", fields=x .})
-- 

-- returns true 9 times out of 10
def still_alive() = URandom()<:0.99

-- 4s avg think time
def think_time() = 10000+Random(6000)

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
   val urls = [
       "http://httpbin.default.svc.cluster.local/get?show_env=1", 
       "http://httpbin.default.svc.cluster.local/delay/2?show_env=1",
       "https://avwx.aeronavmap.com/metar/EDDT",
       "https://avwx.aeronavmap.com/metar/EDDT?options=speech,info,translate,summary",
       "http://httpbin.default.svc.cluster.local/xjson/FB.json",
       "https://edition.cnn.com",
       "http://httpbin.default.svc.cluster.local/status/200"
       ]

  val x = Random(length(urls))
  val url = index(urls, x)
  HTTP(url).get()>res>(if x/=5 && x/= 6 then ReadJSON(res.payload.responseBody) else res.payload.responseBody)

def shuffle(a,b) = if (Random(2) = 1) then (a,b) else (b,a)

def take((a,b)) =    
  a.acquire() >> b.acquireD() ;
  a.release() >> take(shuffle(a,b))
    
def drop(a,b) = (a.release(), b.release()) >> signal


def phil(n,a,b) =
  def think() = 
    if still_alive()
      then
        val clock = Rclock().time
        val w = think_time()
        trace({. event="benchmark_thinking", philosopher=n, thinking=w.})>>
        Rwait(w) >>
        trace({. event="benchmark_rwait", philosopher=n, wait_lag = (clock() * 1000) - w .})
      else
        trace({. event="benchmark_died", philosopher=n, is="☠️.".})>>
        stop
  def can_work() = take((a,b))
  def work() =
    val clock = Rclock().time
    Random(3) >r>
    (if      r=0 then {. f=do_tailmult, s="TAILMULT" .}
     else if r=1 then {. f=do_8queens, s="8QUEENS" .}
     else             {. f=do_http, s="HTTP" .} )  >doit>
    collect(doit.f)>res>
    trace ({. event="benchmark_doit", philosopher=n, done=doit.s, elapsed=clock().})>>
    drop(a,b)
  think() >> can_work() >> work() >> phil(n,a,b)

def philosophers(1,a,b) = phil(1,a,b)
def philosophers(n,a,b) =
  val c = Semaphore(1)
  philosophers(n-1,a,c) | phil(n,c,b)

val fork = Semaphore(1)
philosophers(5,fork,fork)
