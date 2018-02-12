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

def (?)(r) = r.read()

def (:=)(r,v) = r.write(v)

def swap(r,s) = (r?,s?) >(rval,sval)> (r := sval, s := rval) >> signal

