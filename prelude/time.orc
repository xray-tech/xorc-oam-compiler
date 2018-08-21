refer from core((-))
def Rclock () =
  val now = `time.now`()
  def time () = `time.now`() - now
  {. time=time .}
     
def Rwait(x) = Coeffect({. kind = "rwait", delay = x .})
