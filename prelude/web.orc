refer from core ((+), assoc)
def HTTP(uri) =
  def Builder(coef) =

    def header(k, v) =
      Builder(coef + {. headers = assoc(coef.headers, k, v) .})

    def param(k, v) =
      Builder(coef + {. params = assoc(coef.params, k, v) .})

    def get() = Coeffect(coef + {. requestType="get" .})

    def post(payload) =
      Coeffect(coef + {. requestType="post",
                          body=payload .})

    {. header=header, get=get, post=post .}

  Builder({. name="http", uri=uri, headers = {..}, params = {..} .})

def ReadJSON(s) = `web.json_parse`(s)
def WriteJSON(v) = `web.json_generate`(v)
