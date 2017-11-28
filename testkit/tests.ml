type check =
  | AllOf of string list
  | OneOf of string list
[@@deriving variants]

type checks = | Check of check
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

       ("Let(2, 3, 4) >(x,y,z)> Let(x | z)", Check (oneof ["2"; "4"]));

       ("floor(3.2)", Check (allof ["3"]));
       ("ceil(3.2)", Check (allof ["4"]));
       ("sqrt(4)", Check (allof ["2.0"]));
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
     |(-2.3 >-2.3> \"FSuccess\" ; \"Fail\")",
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

         even(1001)", Check (allof ["false"]))]);
  ]
