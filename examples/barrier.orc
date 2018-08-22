def BarrierSync(n) =

  val in =  Semaphore(0)
  val out = Semaphore(0)

  def go() = in.release() >> out.acquire()

  {- Repeat f i times -}
  def repeat(0,_) =  signal
  def repeat(i,f) = f() >> repeat(i-1,f)

  def manager()  =
    repeat(n,in.acquire) >> repeat(n,out.release) >> manager()

  manager()>>{. go=go .}


val barrier = BarrierSync(3).go

  Println(0.1) >> barrier() >> Println(0.2) >> barrier() >> Println(0.3)>> stop
| Println(1.1) >> barrier() >> Println(1.2) >> barrier() >> stop
| Println(2.1) >> barrier() >> Println(2.2) >> stop
