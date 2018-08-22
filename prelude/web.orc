def HTTP(s) =
  val str=s
  def uri() = str
  def get() = Coeffect({. name="http", requestType="get", uri=str .})
  def post(payload,header) =
    Coeffect({. name="http",
                requestType="post",
                uri=uri,
		body=payload .})
  {. uri=uri, get=get, post=post .}

def ReadJSON(s) = `web.json_parse`(s)
def WriteJSON(v) = `web.json_generate`(v)
