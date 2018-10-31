# Differences with Austin TX's implementation.

In the following document, we shall use OAM as short notation for 
[Xray OAM](https://github.com/360dialog/xorc-oam-compiler), and Orc as a short notation for 
[Orc Language v2.1.2](https://github.com/orc-lang/orc/tree/v2.1.2).

We describe here the differences between OAM and Orc.

## host language

|| Orc | OAM|
|----------|-------|----------------------|
| Compiler | `JVM` | `OCaml`  `JavaScript` `JVM` |
| Runtime  | `JVM` | `OCaml` `JVM` `JavaScript` `Elixir` and (partial) `C++`|

## only in Orc

### type system

OAM correctly parses all type annotations supported by Orc except for
[DeclareSignature](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.types.typeinfo.html#ref.types.typeinfo.function) which uses the new keyword `sig` instead of `def`.  Hence the following Orc code:
```
def sort[A](lambda () :: A, lambda (A,A) :: Integer) :: List[A]
def sort(input, comparator) =
  val b = Channel[A]()
  val l = Ref[List[A]]([])
  def sort_aux(A, List[A]) :: List[A]
  def sort_aux(x, []) = [x]
  def sort_aux(x, y:[]) = if (comparator(x, y) <: 0) then x:[y] else y:[x]
  def sort_aux(x, y:yl) = if (comparator(x, y) <: 0) then x:y:yl else y:sort_aux(x, yl)
  def sort_Channel() :: Signal
  def sort_Channel() = (b.get() >x> (l := sort_aux(x, l?))  >> sort_Channel() >> stop); signal
  # (input() >x> b.put(x)  >> stop; b.close()>>stop) | sort_Channel() >> l?
sort(lambda()=(( (1,(2,3)) | (4,true) | (5,[6,7]) | (8,signal) ) >(x,_)> x),
              lambda(x :: Integer, y :: Integer) = x - y)
```
needs to be written:
```
refer from state (Channel,Ref,(?),(:=))
sig sort[A](lambda () :: A, lambda (A,A) :: Integer) :: List[A]
def sort(input, comparator) =
  val b = Channel[A]()
  val l = Ref[List[A]]([])
  sig sort_aux(A, List[A]) :: List[A]
  def sort_aux(x, []) = [x]
  def sort_aux(x, y:[]) = if (comparator(x, y) <: 0) then x:[y] else y:[x]
  def sort_aux(x, y:yl) = if (comparator(x, y) <: 0) then x:y:yl else y:sort_aux(x, yl)
  sig sort_Channel() :: Signal
  def sort_Channel() = (b.get() >x> (l := sort_aux(x, l?))  >> sort_Channel() >> stop); signal
  # (input() >x> b.put(x)  >> stop; b.close()>>stop) | sort_Channel() >> l?
sort(lambda()=(( (1,(2,3)) | (4,true) | (5,[6,7]) | (8,signal) ) >(x,_)> x),
              lambda(x :: Integer, y :: Integer) = x - y)
```
(more on the `refer from` syntax below).

* OAM does not enforce *type judgments*. Indeed, the effort for implementing a type system similar to Orc is not worth the benefits from having it. Instead, we are considering augmenting OAM with a type system similar to this of [Elm](https://guide.elm-lang.org/types/), which will result in us getting rid of type related syntax (e.g., `sig`).

### Data Values 

* OAM does not implement yet [Algebraic Data Types](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.data.datatype.html). We will probably adopt another approach (more on this later).

### Declarations

* OAM does not parse syntax for the following declarations:
   * [def class](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.declarations.defclass.html),
   * [import site](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.declarations.site.html),
   * [import class](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.declarations.class.html),
   * [type](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.declarations.type.html)

### Time

* OAM does not (yet) support [virtual time](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.time.virtual.html).

### Standard Library

* in [idiom](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.idioms.html) OAM no support for `collect`.
* no support for [reflect](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.reflect.html)
* in [state](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.state.html) no support for `Some`, `None`, 
 `BoundedChannel`, `Array`, `ObservationSubject`, `Table`, `Dictionary`, `Interval`, and `Intervals`.
* in [text](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.text.html) only support for `Println`,
* in [time](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.time.html) no support for 
`Rtime` (use `Rclock`),  `Vclock`, `Vawait`, `Vtime` and `IntegerTimeOrder`.
* in [util](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.util.html) no support for
 `UUID`, `Prompt`, `IterableToStream`, `iterableToList`,
`arrayToList`, `listToJavaList`, `listToArray`, `fillArray`, `sliceArray`, `takePubs`, 
`withLock`, `synchronized` and `InvokeExecutable`.

## only in Xray OAM

### compiler 
Invoking the compiler (usually `orc.exe`) without options prints the following:
```
Orc programming language compiler and VM

  orc.exe SUBCOMMAND

=== subcommands ===

  compile       produce bytecode
  exec          executes orc
  tests-server  tests-server. supports TestKit protocol
  unblock       continue execution of serialized orc program
  version       print version information
  help          explain a given subcommand (perhaps recursively)
  ```
  Each subcommand has its own help. E.g., `orc.exe exec -h` prints:
 ```
  executes orc

  orc.exe exec [INPUT]

=== flags ===

  [-bc]             Execute bytecode, not Orc source file. By default reads from
                    stdin
  [-debugger]       Run with debugger
  [-dump Path]      to store intermediate state if any
  [-ext]            Extensions
  [-i Directories]  to include
  [-prelude]        Implicity refer whole prelude
  [-verbose]        Verbose logging
  [-help]           print this help text and exit
                    (alias: -?)
```
The `-i DIR` will cause the compiler to add all `DIR/*.orc` to its compilation unit.  
The `-prelude` option causes most functions in the standard library to be available without having to `refer` them, currently:
* from [core](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.core.html) 
               `abs` `signum` `min` `max` `+` `-` `*` `/` `%` `**` `=` `/=` `:>` 
               `>=` `<:` `<=` `||` `&&` `~` `:` `Ift` `Iff` `ceil`  `floor` `sqrt` `Let` `Println`.
* from [idioms](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.idioms.html) 
               `curry` `curry3` `uncurry` `uncurry3` `flip` `constant` `defer`
               `defer2` `ignore` `ignore2` `compose` `while` `repeat` `fork`
               `forkMap` `seq` `seqMap` `join` `joinMap` `alt` `altMap` `por`
               `pand`.
* from [list](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.list.html) 
             `each` `map` `reverse` `filter` `head` `tail` `init` `last`
             `empty` `index` `append` `foldl` `foldl1` `foldr` `foldr1`
             `afold` `zipWith` `zip` `unzip` `concat` `length` `take` `drop`
             `member` `merge` `mergeBy` `sort` `sortBy` `mergeUnique`
             `mergeUniqueBy` `sortUnique` `sortUniqueBy` `group` `groupBy`
             `rangeBy` `range` `any` `all` `sum` `product` `and` `or`
             `minimum` `maximum`.
* from [state](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.state.html) 
             `Cell` `Channel` `Ref` `?` `:=`.
* from [time](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.time.html) 
             `Rwait`
* from [util](https://orc.csres.utexas.edu/documentation/html/refmanual/ref.stdlib.util.html) 
             `for` `upto`.



### new syntax: `refer from`

OAM introduces the  `refer from` new syntax, which *selectively* brings into scope functions defined somewhere else. Currently OAM compiler accepts a `-i DIR` option, which will cause it to read all the files with extension `.orc` in `DIR`. 
Given:
```
mkdir DIR && echo "def bar() = 1|2|3|4" > DIR/foo.orc
```
on has:
```
echo "refer from foo (bar) \n bar()" | orc.exe exec -i DIR
 [INFO] Value: 4
 [INFO] Value: 3
 [INFO] Value: 2
 [INFO] Value: 1
```


