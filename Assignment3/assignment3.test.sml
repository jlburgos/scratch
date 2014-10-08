(* Testing Assignment 3 methods *)

use "assignment3.sml";

(* Problem Num 1 *)
val p1a = [] = (only_capitals [])
val p1b = [] = (only_capitals ["lower"])
val p1c = ["Upper"] = (only_capitals ["lower","Upper"])

(* Problem Num 2 *)
val p2a = "First" = (longest_string1 ["dumb","First","first","last"])

(* Problem Num 3 *)
val p3a = "first" = (longest_string2 ["dumb","First","first","last"])

(* Problem Num 4 *)
(* Testing longest_string3 *)
val p4a = "First" = (longest_string3 ["dumb","First","first","last"])

(* Testing longest_string4 *)
val p4b = "first" = (longest_string4 ["dumb","First","first","last"])

(* Problem Num 5 *)
val p5a = "Longest2" = (longest_capitalized ["Longes","Longest1","Longest2"])

(* Problem Num 6 *)
val p6a = "fdsa" = (rev_string "asdf")

(* Problem Num 7 *)
val p7a = 1 = (first_answer (fn x => if x=1 then SOME x else NONE) [1,2,3])
val p7b = [1] = (first_answer (fn x => if x=1 then SOME[x] else NONE) [1,2,3])

(* Problem Num 8 *)
val p8a = SOME[1,2,3,4] = (all_answers (fn x => if x<=4 then SOME[x] else NONE)
[1,2,3,4])
val p8b = NONE = (all_answers (fn x => if x<=4 then SOME[x] else NONE)
[1,2,3,4,5])
val p8c = SOME[] = (all_answers (fn x => if x<=4 then SOME[x] else NONE) [])
val p8d = SOME[[1],[1],[1]] = (all_answers (fn x => if x=[1] then SOME[x] else
  NONE) [[1],[1],[1]])
val p8e = NONE = (all_answers (fn x => if x=1 then SOME[x] else NONE) [2,3,4])
val p8f = SOME[1,1,1]=(all_answers (fn x=> if (length x)=1 then SOME x else NONE)
          [[1],[1],[1]])
(* Problem Num 9 *)
val p9a = 1 = (count_wildcards Wildcard)
val p9b = 3 = (count_wildcards (TupleP [Wildcard,(ConstructorP("asdf",Wildcard)),
                                        Wildcard]))
val p9c = 2 = (count_wild_and_variable_lengths (Variable("ad")))
val p9d = 0 = (count_some_var ("asdf",Variable("ASDF")))
val p9e = 2 = (count_some_var ("asdf",
  TupleP [Variable("asdf"),ConstP(5),UnitP,
          Variable("Asdf"),Variable("asdf")]))

(* Problem Num 10 *)
val p10a = false = (check_pat (TupleP[Variable("asdf"),Variable("asdf")]))
val p10b = true = (check_pat (TupleP[Variable("Asdf"),Variable("asdf")]))

(* Problem Num 11 *)
val p11a = NONE = (match(Const(1), UnitP))
val p11b = SOME[("x",Const(10))] = (match(Const(10),Variable("x")))

(* Problem Num 12 *)
val p12a = SOME[] = (first_match(Unit,[UnitP]))
val p12b = NONE = (first_match(Const(3), [ ConstructorP("asdf",ConstP(2)) ]))
val p12c = SOME[("asdf",Const(3))] =
  (first_match(Const(3), [ ConstructorP("asdf",ConstP(2)),
                           Variable("asdf")]))

