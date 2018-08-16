refer from core ((=))

def Ref(init) =
  val v = `core.make-ref`(init)
  def read() =
    `core.deref`(v)
  def write(x) =
    `core.set`(v, x)
  {. read = read, write = write .}

def Cell() =
  val v = `core.make-pending`()
  def read() =
    `core.pending-read`(v)
  def readD() =
    if `core.is-realized`(v)
    then `core.pending-read`(v)
    else stop
  def write(x) =
    if `core.is-realized`(v)
    then stop
    else `core.realize`(v, x)
  {. read = read, readD = readD, write = write .}

def Channel() =
  refer from list (empty, head, tail, append)
  val queue = `core.make-ref`([])
  val emptyWait = `core.make-ref`(null)
  val readPoint = `core.make-ref`(null)
  val isClosed = `core.make-ref`(false)
  def getD() =
    val queue' = `core.deref`(queue)
    if empty(queue')
    then stop
    else
      val queue'' = tail(queue') #
      (if empty(queue'')
       then
       (val emptyWait' = `core.deref`(emptyWait)
        if emptyWait' = null
        then signal
        else `core.realize`(emptyWait', signal))
       else signal)
      >> `core.set`(queue, queue'')
      >> head(queue')

  def pendingCreateIfNeedAndRead(box) =
    (val pend = `core.deref`(box)
     if pend = null
     then
       `core.make-pending`()
       >pend> `core.set`(box, pend)
       >> `core.pending-read`(pend)
     else `core.pending-read`(pend))

  def get() =
    getD(); if `core.deref`(isClosed) = true then stop else (pendingCreateIfNeedAndRead(readPoint) >> get())

  def put(v) =
    `core.set`(queue, append(`core.deref`(queue), [v])) >>
    if `core.deref`(isClosed) = true
    then stop
    else
    (val readPoint' = `core.deref`(readPoint)
     if readPoint' = null
     then signal
     else `core.set`(readPoint, null) >> `core.realize`(readPoint', signal))    

  def close() =
    `core.set`(isClosed, true)
    >>
    val readPoint' = `core.deref`(readPoint) #
    (if readPoint' = null
    then signal
    else `core.stop-pending`(readPoint'))
    >>
    if empty(`core.deref`(queue))
    then signal
    else pendingCreateIfNeedAndRead(emptyWait)
  {. get = get, put = put, close = close .}

def Counter() =
  refer from core ((+), (-))
  val value = `core.make-ref`(0)
  val zeroWait = `core.make-ref`(`core.make-pending`())

  def inc() =
    `core.set`(value, `core.deref`(value) + 1)#

  def dec() =
     val value' = `core.deref`(value)
     if value' = 0
     then stop
     else
       val value'' = value' - 1
       `core.set`(value, value'') >>
       (if value'' = 0
        then `core.realize`(`core.deref`(zeroWait), signal)
             >> `core.set`(zeroWait, `core.make-pending`())
        else signal)

  def onZero() =
    val value' = `core.deref`(value)
    if value' = 0
    then signal
    else 
      `core.pending-read`(`core.deref`(zeroWait))

  def getValue() = `core.deref`(value)

  {. inc = inc, dec = dec, onZero = onZero, value = getValue .}

def Semaphore(n) =
  val b = Channel()
  def acquire() = b.get()
  def acquireD() = b.getD()
  def release() = b.put(signal)
  def add(0) = signal
  def add(i) = release() >> add(i-1)
  add(n) >> {. acquire = acquire, acquireD = acquireD, release = release .}

def (?)(r) = r.read()

def (:=)(r,v) = r.write(v)

def swap(r,s) = (r?,s?) >(rval,sval)> (r := sval, s := rval) >> signal

def fst((x,_)) = x

def snd((_,y)) = y
