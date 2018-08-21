# xorc-oam-compiler

This is Xorc implementation of [Orc Language](https://orc.csres.utexas.edu/) version 2.1.2 with some extensions.


## ORC resources

- [Orc in 15 mins](https://orc.csres.utexas.edu/tutorial.shtml)
- [Reference](https://orc.csres.utexas.edu/documentation/html/refmanual/index.html)




## Requirements

[OPAM](https://opam.ocaml.org/doc/Install.html)

```bash
# macOS
brew install opam

# Debian 
apt-get install opam

opam switch 4.05.0
eval `opam config env`

opam install jbuilder menhir core async async_extended msgpck sedlex ppx_jane benchmark
```

## How to build

```bash
jbuilder build bin/orc.exe

# builds in ./_build/default/bin/orc.exe

```

## Compile & Exec

```bash
# compile
orc.exe compile examples/par.orc -output examples/par.orc.bc

# execute
orc.exe exec -bc examples/par.orc.bc

# or directly compile & execute
orc.exe exec examples/par.orc
```


## How to run tests

```bash
jbuilder build testkit/testkit.exe

./_build/default/testkit/testkit.exe exec ./_build/default/bin/orc.exe -- tests-server
```

## diff Austin

TODO: describe Xorc extensions and differences from Orc Austin 2.1.2



