To build and run

```
jbuilder build c/main.cmx src/orcml.cmxa
ocamlc -g -c c/init.c && mv init.o c/init.o
ocamlfind opt -o c/liborcml.so -linkpkg -runtime-variant _pic -verbose -ccopt -dynamiclib -package base,menhirLib,sedlex,stdio,msgpck,sexplib,expect_test_helpers c/init.o  _build/default/src/orcml.cmxa _build/default/c/main.cmx
clang -g -Wall -Wextra -c -I`ocamlc -where` -c c/main.c -o c/main.o
clang -o c/main c/main.o -Wl,-rpath,. -L. -lorcml

./c/main examples/par.orc
```
