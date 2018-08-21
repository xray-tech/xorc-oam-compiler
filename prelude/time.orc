refer from core((-))
def Rclock () =
  val now = Coeffect({. kind="now" .})
  def time () =  Coeffect({. kind="now" .}) - now
  now>>{. time=time .} --
     
def Rwait(x) = Coeffect({. kind = "rwait", delay = x .})
