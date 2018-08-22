{-
 - Adapted from https://github.com/orc-lang/orc/blob/v2.1.2/OrcExamples/synchronization/readers-writers.orc
 - see phil().think for a rough tuning of run duration
 -}
def shuffle(a,b) = if (Random(2) = 1) then (a,b) else (b,a)

def take((a,b)) =    
  a.acquire() >> b.acquireD() ;
  a.release() >> take(shuffle(a,b))
    
def drop(a,b) = (a.release(), b.release()) >> signal

def tailmult(a,b) =
  def tailm(0,acc) = acc
  def tailm(a,acc) = tailm(a-1,acc+b)
  tailm(a,0)

def load_it() =
   val n = 3+Random(3)
   if n=3 then       tailmult(2, 30003) | tailmult(20002, 4) | tailmult(10000,123)
   else if n=4 then  tailmult(2, 30003) | tailmult(20002, 4) | tailmult(10000,123) 
                     | tailmult(10000,1234)
   else if n=5 then  tailmult(22, 30003) | tailmult(20002, 4) | tailmult(10000,123) 
                     | tailmult(10000,1234) | tailmult(16000,1234)
   else              tailmult(2, 3003) | tailmult(20002, 4) | tailmult(10000,123) 
                     | tailmult(1200,1234) | tailmult(18000,1234) | tailmult(10000,1234) 

def phil(n,a,b) =
  def think() = 
    Println("thinking: "+n) >> 
    -- (if (Random(10) <: 9)
    (if (Random(100) <: 99)
      then Rwait(1500+Random(4000)) -- 3.5 secons average
      else Println("phil="+n+" stop")>>stop)
  def can_work() = take((a,b))
  def work() = 
    Println("working: "+n) >> 
    collect(load_it)>l>Println(l)>>
    Println("done working:" +n) >> 
    drop(a,b)#
  think() >> can_work() >> work() >> phil(n,a,b)

def philosophers(1,a,b) = phil(1,a,b)
def philosophers(n,a,b) =
  val c = Semaphore(1)
  philosophers(n-1,a,c) | phil(n,c,b)

val fork = Semaphore(1)
philosophers(5,fork,fork)
