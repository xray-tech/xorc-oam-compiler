refer from core((-))
def Rclock () =
  def clock() = Coeffect({. name="clock", kind="MONOTONIC" .})
  val now = clock()
  def time () =  clock() - now
  now>>{. time=time .} --
     
def Rwait(x) = Coeffect({. name = "rwait", delay = x .})
