refer from core((-))
def Rclock () =
  def clock() = Coeffect({. name="clock", kind="MONOTONIC" .})
  val now = clock()
  def time () =  clock() - now
  {. time=time .} --
     
def Rwait(x) = Coeffect({. name = "rwait", delay = x .})

-- def metronome(Integer) :: Signal
def metronome(t) = signal | Rwait(t) >> metronome(t)

