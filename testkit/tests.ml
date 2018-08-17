type check =
  | AllOf of string list
  | OneOf of string list
[@@deriving variants]

type checks = | Check of check
              | WithKilled of { values : check;
                                killed : int list }
              | CheckAndResume of { values : check;
                                    unblock : (int * string);
                                    killed : int list;
                                    next : checks}

let tests =
  [("arithmetic",[
       ("1 + 2", Check (allof ["3"]));
       ("7 * 3", Check (allof ["21"]));
       ("30 / 6", Check (allof ["5"]));
       ("5 - 4", Check (allof ["1"]));
       ("5 - 4", Check (allof ["1"]));
       ("(95 + 4 + 1) * 21", Check (allof ["2100"]));
       ("23 = 20 + 3", Check (allof ["true"]));
       ("(7 >= 2 + 4)", Check (allof ["true"]));
       ("15 % 4", Check (allof ["3"]));
       ("2**8", Check (allof ["256"]));
       ("2 ** 3 ** 2", Check (allof ["512"]));

     ]);
   ("prelude", [
       ("true && (true || false)", Check (allof ["true"]));
       ("\"All your \" + \"base are \" + \"belong to us\"",
        Check (allof ["\"All your base are belong to us\""]));
       ("7 = 7", Check (allof ["true"]));
       ("278 /= 290", Check (allof ["true"]));
       ("6 <: 10", Check (allof ["true"]));
       ("23 :> 900", Check (allof ["false"]));
       ("8 <= 2", Check (allof ["false"]));
       ("(7 >= 2 + 4)", Check (allof ["true"]));
       ("98 >= 98", Check (allof ["true"]));
       ("false && true", Check (allof ["false"]));
       ("false || true", Check (allof ["true"]));
       ("~true", Check (allof ["false"]));
       ("86 = true", Check (allof ["false"]));
       ("3 :> 2", Check (allof ["true"]));
       ("6 <: 7", Check (allof ["true"]));

       ("(7, 3, 2)", Check (allof ["(7, 3, 2)"]));
       ("[8, 9, 10]", Check (allof ["[8, 9, 10]"]));
       ("if false then 7 else 8", Check (allof ["8"]));

       ("val b = true
      Ift(b) >> \"true\" | Iff(b) >> \"false\"", Check (allof ["\"true\""]));

       ("val n = 5
      if n :> 0 then n-1 else n+1", Check (allof ["4"]));

       ("val x = 22
      x", Check (allof ["22"]));
       ("signal", Check (allof ["signal"]));
       ("stop", Check (allof []));

       ("(2, 3, 4) >(x,y,z)> Let(x | z)", Check (oneof ["2"; "4"]));

       ("floor(3.2)", Check (allof ["3"]));
       ("ceil(3.2)", Check (allof ["4"]));
       ("sqrt(4)", Check (allof ["2.0"]));
       ("Let(1, 2)", Check (allof ["1"]));
     ]);
   ("comments", [
       ("{- I
         am
         a
         multi-line
         comment
       -}

      {- {- I am a nested comment -} -}

      -- I am a single-line comment

      1 + {- I am hidden in an expression -} 1",
        Check (allof ["2"]))]);
   ("data-structures",[
       ("val empty = {.  .}
        val one = {. x = 1 .}
        val two = {. y = 3, z = true .}

        empty
        | one
        | (one.x)
        | (two.y)
        | (two.z)",
        Check (allof ["{. .}"; "{. x = 1 .}"; "1"; "3"; "true"]));
       ("val a = {. x = 0 .} + {.  .}
      val b = {.  .} + {. y = 1 .}
      val c = {. x = 10 .} + {. x = 2 .}
      val d = {. x = 11, y = 4 .} + {. z = 5, x = 3 .}
      val e = {. x = {. y = 12 .}, z = 13 .} + {. x = {. y = 6, z = 7 .}, y = 8, z = 9 .}

      a.x | b.y | c.x | d.x | d.y | d.z | e.x.y | e.x.z | e.y | e.z",
        Check (allof ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]));
       ("[1,2,3] | [1]:4:[6] | 1:5:8:[] | [] | [7]",
        Check (allof ["[1,2,3]"; "[[1], 4, 6]"; "[1, 5, 8]"; "[]"; "[7]"]));
     ]);
   ("patterns", [
       ("( (1,4) | (2,5) | (1,6) )  >(1,x)> x",
        Check (allof ["4"; "6"]));

       ("x <(1,x)<  ( (2,5) | (1,4) | (1,6) )",
        Check (oneof ["4"; "6"]));

       ("( (1,2) | (1,2,3) | (5,6) | (5,6,7) ) >(x,y)> (y,x)",
        Check (allof ["(2,1)"; "(6,5)"]));

       ("( [1,2,3] | [4,5] | [6] | [] ) >a:b:c> (a,b,c)",
        Check (allof ["(1,2,[3])"; "(4,5,[])"]));

       ("[1,2,3,4] >a:t> t >b:c:u> (u,c,b,a)",
        Check (allof ["([4],3,2,1)"]));

       ("(-1 >-1> \"ISuccess\" ; \"Fail\")
         |
         (-2.3 >-2.3> \"FSuccess\" ; \"Fail\")",
        Check (allof ["\"ISuccess\""; "\"FSuccess\""]));

       ("( (1,(2,3)) | (4,(5,6)) ) >(a,(b,c) as d)> (a,b,c,d)",
        Check (allof ["(1,2,3,(2,3))"; "(4,5,6,(5,6))"]));

       ("( (1,(2,3)) | (4,true) | (5,[6,7]) | (8,signal) ) >(x,_)> x",
        Check (allof ["1"; "4"; "5"; "8"]));

       ("(1, [2,3], (4, 5)) > (a, b:_, (c,_)) > (a,b,c)",
        Check (allof ["(1,2,4)"]));

       ("{. a = 1, b = [2,3], c = {. d =  4 .} .} > {. a = a, b = b:_, c = {. d = d .} .} > (a,b,d)",
        Check (allof ["(1,2,4)"]));

     ]);
   ("stop-semantic", [
       ("((1 | stop) | stop); 3", Check (allof ["1"]));
       ("((1 >> stop) | stop); 3", Check (allof ["3"]));
       ("(1 << stop); 3", Check (allof ["1"]));
       ("(stop << 1); 3", Check (allof ["3"]));
       ("(1 | stop) >> 2", Check (allof ["2"]));
       ("(1;signal) >> stop; 3", Check (allof ["3"]));
       ("def f(x) = x # (f(x) <x< stop); 5", Check (allof ["5"]));
       ("val x = stop val y = Coeffect(1) x+y;y+x;true",
        WithKilled {values = allof ["true"]; killed = [0]})
     ]);
   ("combinators", [
       ("2 | 3", Check (allof ["2"; "3"]));
       ("2 + 3 >x> x", Check (allof ["5"]));
       ("y <y< (4 | 6)", Check (oneof ["4"; "6"]));
       ("stop ; 5 + 9", Check (allof ["14"]));
       ("(Ift(b) >> 1 | Ift(~b) >> 0) <b< true", Check (allof ["1"]));
       ("1 + ( 2 | 3 )", Check (oneof ["3"; "4"]));
       ("def picknum() = x <x< ( 1 | 2 )
      picknum()", Check (oneof ["1"; "2"]));
       ("def pubNums(n) = if(n :> 0) then (n | pubNums(n-1)) else stop
      pubNums(5) >x> x", Check (allof ["5"; "4"; "3"; "2"; "1"]));
       ("1 >> (if true then 2 else 3)", Check (allof ["2"]));
     ]);
   ("defs", [
       ("def f(a, b) = a + 2 * b
         f(1, 2)",
        Check (allof ["5"]));

       ("val (x,_,_) = (1,(2,2),[3,3,3])
         x",
        Check (allof ["1"]));

       ("def onetwosum(f) = f(1) + f(2)
         onetwosum( lambda(x) = x * 3 )",
        Check (allof ["9"]));

       ("def pow2(0) = 1
         def pow2(n) = 2 * pow2(n-1)
         pow2(8)",
        Check (allof ["256"]));

       ("def even(0) = true
         def odd(0) = false
         def even(n) = (if n :> 0 then n-1 else n+1) >i> odd(i)
         def odd(n) = (if n :> 0 then n-1 else n+1) >i> even(i)
         even(9)",
        Check (allof ["false"]));

       ("def sumto(n) = if n <: 1 then 0 else n + sumto(n-1)
         sumto(10)",
        Check (allof ["55"]));

       ("def Sum(a) = ( lambda(b) = a+b )
         val f = Sum(3)
         f(4)",
        Check (allof ["7"]));

       ("def foo(x) =
         def bar(y) = x + y
         bar
         foo(10)(20)",
        Check (allof ["30"]));

       ("def sum(a) = ( lambda(b) = a+b )
         val f = sum(3)
         f(4)",
        Check (allof ["7"]));

       ("def foo((1, x)) = x foo((2, 1)) | foo((1,2))",
        Check (allof ["2"]));

       ("def tailrec(0) = 0 def tailrec(x) = tailrec(x - 1) tailrec(1)",
        Check (allof ["0"]));

       ("def f(0) if (true) = 0
         def f(x) = x+1 #
         def g(x:xs) if (true) = x
         f(0) | g([1,2])",
        Check (allof ["0"; "1"]));

       ("def fib(0) = 0
        def fib(1) = 1
        def fib(n) if (n :> 1) = fib(n-1) + fib(n-2)
        fib(-1) ; fib(3)",
        Check (allof ["2"]));

       ("def fact(n) if (n :> 0) = n * fact(n-1)
         def fact(0) = 1
         fact(-1) ; fact(0) | fact(4)",
        Check (allof ["1"; "24"]));

       ("def f(x) if (stop) = x
         def f(x) = x+1
         f(0)",
        Check (allof ["1"]));

       ("def findCube(i, x) if (i*i*i <: x) = findCube(i+1, x)
         def findCube(i, x) if (i*i*i = x) = i
         def findCube(i, x) if (i*i*i :> x) = stop {- unneeded, but helps improve readability -}
         # (63|64|65) >i> findCube(0,i)",
        Check (allof ["4"]));
       ("def foo(a, b) = a + b
         def bar(f) = f(1,2)
         bar(foo)",
        Check (allof ["3"]));

       ("def foo(x) =
           def bar() = x + 1
           def zoo() = bar()
           zoo()
        foo(1)",
        Check (allof ["2"]))
     ]);
   ("blocks", [
       (* ("Coeffect(1) >x> x + 2",
        *  CheckAndResume { values = allof [];
        *                   unblock = (0, "1");
        *                   killed = [];
        *                   next = Check (allof ["3"])});
        *
        * ("Coeffect(1) >x>
        *  (val y = 3
        *   x + y)",
        *  CheckAndResume { values = allof [];
        *                   unblock = (0, "4");
        *                   killed = [];
        *                   next = Check (allof ["7"])});
        *
        * ("Coeffect(1) >(x, y)> x + y",
        *  CheckAndResume { values = allof [];
        *                   unblock = (0, "(1,2)");
        *                   killed = [];
        *                   next = Check (allof ["3"])});
        *
        * ("Coeffect(1) >x> (Coeffect(2), x) >z> z",
        *  CheckAndResume
        *    { values = allof [];
        *      unblock = (0, "\"a\"");
        *      killed = [];
        *      next = CheckAndResume
        *          { values = allof [];
        *            unblock = (1, "\"b\"");
        *            killed = [];
        *            next = Check (allof ["(\"b\", \"a\")"])}}); *)

       ("val x = Coeffect(1) | Coeffect(2) x",
        CheckAndResume { values = allof [];
                         unblock = (0, "2");
                         killed = [1];
                         next = Check (allof ["2"])});

       ("val (1, y) = Coeffect(1) | Coeffect(2)
        y",
        CheckAndResume
          { values = allof [];
            unblock = (0, "(2, 4)");
            killed = [];
            next = CheckAndResume
                { values = allof [];
                  unblock = (1, "(1,3)");
                  killed = [];
                  next = Check (allof ["3"])}});
       ("(val a = Coeffect(1) | Coeffect(2) a + 1 >> stop); 3",
        CheckAndResume { values = allof [];
                         unblock = (0, "0");
                         killed = [1];
                         next = Check (allof ["3"])});
       ("def foo(0) = 1
         def foo((1, x)) = x
         foo(Coeffect(1)) | foo(Coeffect(2)) | foo(Coeffect(3))",
        CheckAndResume
          { values = allof [];
            unblock = (0, "0");
            killed = [];
            next = CheckAndResume
                { values = allof ["1"];
                  unblock = (1, "(2, 2)");
                  killed = [];
                  next = CheckAndResume
                      { values = allof [];
                        unblock = (2, "(1, 3)");
                        killed = [];
                        next = Check (allof ["3"])}}});
       ("val x = Coeffect(1)
         val y = x | Coeffect(2)
         y",
        CheckAndResume
          { values = allof [];
            unblock = (1, "1");
            killed = [0];
            next = Check (allof ["1"])});]);
   ("tailrec", [
       ("def tailrec(0) = 0 def tailrec(x) = tailrec(x - 1) tailrec(10000)",
        Check (allof ["0"]));
       ("def tailrec(0) = 0 def tailrec(x) = x - 1 >y> tailrec(y) tailrec(10000)",
        Check (allof ["0"]));
       ("def tailrec(0) = 0 def tailrec(x) = x - 1 >y> y >y> tailrec(y) tailrec(10000)",
        Check (allof ["0"]));
       ("def tailrec(0) = 0 def tailrec(x) = stop | tailrec(x - 1) tailrec(10000)",
        Check (allof ["0"]));
       ("def tailrec(0) = 0 def tailrec(x) = val y = x - 1 stop | tailrec(y) tailrec(10000)",
        Check (allof ["0"]));

       ("def f(x)=if (x<:10000) then (stop|f(x+1)) else stop\n f(0)",
        Check (allof []));
       ("def tailadd(0,n) = n def tailadd(a,b) = tailadd(a-1,b+1) tailadd(40001, 10004)",
        Check (allof ["50005"]));
       ("def tailmult(a,b) =
             def tailm(0,acc) = acc
             def tailm(a,acc) = tailm(a-1,acc+b)
             tailm(a,0)
         tailmult(2, 30003) | tailmult(20002, 4)"),
       Check (allof ["60006"; "80008"])
     ]);
   ("basic", [
       (* ("1 | 2", Check (allof ["1"; "2"]));
        * ("1 + Coeffect(1)", CheckAndResume
        *    { values = allof [];
        *      unblock = (0, "2");
        *      killed = [];
        *      next = Check (allof ["3"])});
        * ("2 | 3 >x> (def foo() = x + 1 foo())",
        *  Check (allof ["3"; "4"])); *)
       ("(2 | 3) >x> def foo() = x + 1 # foo() + Coeffect(1)",
        CheckAndResume { values = allof [];
                         unblock = (0, "1");
                         killed = [];
                         next = CheckAndResume { values = allof ["4"];
                                                 unblock = (1, "1");
                                                 killed = [];
                                                 next = Check (allof ["5"])}});
       ("def bar(f) = f(3) + Coeffect(1) #
         (1 | 2) >x> def foo(y) = x + y# bar(foo)",
        CheckAndResume { values = allof [];
                         unblock = (0, "1");
                         killed = [];
                         next = CheckAndResume { values = allof ["5"];
                                                 unblock = (1, "1");
                                                 killed = [];
                                                 next = Check (allof ["6"])}});
       ("def even(x) = if x = 0 then true else odd(x - 1)
         def odd(x) = if x = 0 then false else even(x - 1)

         even(101)", Check (allof ["false"]))]);
   ("regressions",
    (* Tricky case. We return closure with unrealized values inside *)
    [("def closureBuilder() =
           val s = Coeffect(1) >> 1
           def closure() = s
           closure
         closureBuilder()()",
      CheckAndResume
        { values = allof [];
          unblock = (0, "signal");
          killed = [];
          next = Check (allof ["1"])});
     ("def c1() =
       def c() = 1
         c
       def c2() =
         def c() = 2
       c
       c1()() | c2()()",
      Check (allof ["1"; "2"]))]);
   ("benchs", [
       ("def fact(n) = if (n :> 0) then n * fact(n-1) else 1

fact(10)",
        Check (allof ["3628800"]));
       ("def fact(x) =
  def step(n, acc) =
    if (n :> 0) then step(n-1, n * acc) else acc
  step(x, 1)

fact(10)",
        Check (allof ["3628800"]))
     ])
  ]
