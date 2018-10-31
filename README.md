# xorc-oam-compiler

This is Xorc implementation of [Orc Language](https://orc.csres.utexas.edu/) version 2.1.2 with some restrictions and extensions.


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

opam install dune menhir core async async_extended msgpck sedlex ppx_jane benchmark
```

## How to build

```bash
dune build bin/orc.exe

# builds in ./_build/default/bin/orc.exe

```

## Compile & Exec

```bash
# compile
./_build/default/bin/orc.exe compile examples/par.orc -output examples/par.orc.bc

# execute
./_build/default/bin/orc.exe exec -bc examples/par.orc.bc

# or directly compile & execute
./_build/default/bin/orc.exe exec examples/par.orc
```


## How to run tests

Tests are done using the `TestKit` protocol (TODO document TestKit)

```bash
dune build testkit/testkit.exe
```

### How to run tests against the OCaml OAM

```bash
./_build/default/testkit/testkit.exe exec ./_build/default/bin/orc.exe -- tests-server
```

### How to run tests against the Kotlin OAM

Build [the Kotlin OAM](https://github.com/xray-tech/xorc-kotlin-oam)


```bash
./_build/default/testkit/testkit.exe exec java -- -jar /path/to/kotlin-oam/bin/build/libs/jvm-oam-1.1-SNAPSHOT-all.jar repl
```

## Austin TX's implementation

See the [difference document](https://github.com/xray-tech/xorc-oam-compiler/blob/master/Compatibility.md) for a description of the compability between xorc.io and [Orc 2.1.2](https://github.com/orc-lang/orc/tree/v2.1.2) from the [Orc Project](https://orc.csres.utexas.edu) at UTexas.



