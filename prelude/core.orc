def (+)(x,y) = `core.add`(x, y)
def (-)(x,y) = `core.sub`(x, y)
def (*)(x,y) = `core.mult`(x, y)
def (/)(x,y) = `core.div`(x, y)
def (%)(x,y) = `core.mod`(x, y)
def (**)(x,y) = `core.pow`(x, y)
def (=)(x,y) = `core.eq`(x, y)
def (/=)(x,y) = `core.not-eq`(x, y)
def (:>)(x,y) = `core.gt`(x, y)
def (>=)(x,y) = `core.gte`(x, y)
def (<:)(x,y) = `core.lt`(x, y)
def (<=)(x,y) = `core.lte`(x, y)
def (||)(x,y) = `core.or`(x, y)
def (&&)(x,y) = `core.and`(x, y)
def (:)(x,y) = `core.cons`(x, y)
def (~)(x) = `core.not`(x)
def Ift(x) = `core.ift`(x)
def Iff(x) = `core.iff`(x)

def ceil(x) = `core.ceil`(x)
def floor(x) = `core.floor`(x)
def sqrt(x) = `core.sqrt`(x)
def Let(x) = `core.let`(x)

def Println(x) = Coeffect({. name = "println", value = x .})
def Error(x) = Println("Error: " + x)

def abs(x) = if x <: 0 then -x else x

def signum(x) =
  if x <: 0 then -1
  else if x :> 0 then 1
  else 0

def min(x,y) = if y <: x then y else x

def max(x,y) = if x :> y then x else y

def eq(x,y) = (x = y)

def assoc(m, k, v) = m + `core.make-record`(k, v)
def get(m, k) = `core.field-access`(m, k)

