FROM xorcio/opam as builder

# explicit ppx_tools_versioned.5.2 is workaround for https://github.com/alainfrisch/sedlex/issues/64
RUN opam repository add remote https://opam.ocaml.org && opam update && opam install -y dune menhir core async async_extended msgpck "sedlex<2.0.0" ppx_jane benchmark yojson mtime ppx_tools_versioned.5.2

COPY . /workspace

WORKDIR /workspace

RUN opam config exec -- dune build bin/orc.exe testkit/testkit.exe

FROM debian:stable-slim

COPY --from=builder /workspace/_build/default/bin/orc.exe /workspace/_build/default/testkit/testkit.exe /usr/local/bin/

COPY --from=builder /workspace/prelude /prelude

CMD ["orc.exe"]
