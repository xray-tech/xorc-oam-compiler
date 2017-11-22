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
      Ift(b) >> \"true\" | Iff(b) >> \"false\"", Check (allof ["true"]));

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
   ("basic", [
       ("1 | 2", Check (allof ["1"; "2"]));
       ("1 + Coeffect(1)", CheckAndResume
          { values = allof [];
            unblock = (0, "2");
            killed = [];
            next = Check (allof ["3"])})]);]
