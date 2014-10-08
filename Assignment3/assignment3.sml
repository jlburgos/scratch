(* Homework Number 3 *)

exception NoAnswer

datatype pattern = Wildcard
  | Variable of string
  | UnitP
  | ConstP of int
  | TupleP of pattern list
  | ConstructorP of string * pattern

datatype valu = Const of int
  | Unit
  | Tuple of valu list
  | Constructor of string * valu

fun g f1 f2 p =
  let val r = g f1 f2 in
    case p of
         Wildcard => f1()
       | Variable x => f2 x
       | TupleP ps => List.foldl (fn (p,i) => (r p) + i) 0 ps
       | ConstructorP(_,p) => r p
       | _ => 0
  end

(**** For the Challenge Problem Only ****)

datatype typ = Anything
  | UnitT
  | IntT
  | TupleT of typ list
  | Datatype of string

(**** Begin Assignment Here ****)

(* Problem Num 1 *)
val only_capitals = List.filter (fn str => Char.isUpper (String.sub(str,0)))

(* Problem Num 2 *)
val longest_string1 =
  List.foldl (fn (str,acc) => if (String.size str) > (String.size acc)
                              then str else acc) ""

(* Problem Num 3 *)
val longest_string2 =
  List.foldl (fn (str,acc) => if (String.size str) >= (String.size acc)
                              then str else acc) ""

(* Problem Num 4 *)
fun longest_string_helper cmp =
  List.foldl (fn (str,acc) => if cmp (String.size acc, String.size str)
                              then str else acc) ""

val longest_string3 = longest_string_helper (fn (x,y) => y>x)
val longest_string4 = longest_string_helper (fn (x,y) => y>=x)

(* Problem Num 5 *)
val longest_capitalized = (longest_string2 o only_capitals)

(* Problem Num 6 *)
val rev_string = (implode o rev o explode)

(* Problem Num 7 *)
fun first_answer f xs = case xs of
                             [] => raise NoAnswer
                           | x::xs' => case (f x) of
                                            NONE => (first_answer f xs')
                                          | SOME x => x

(* Problem Num 8 *)
fun all_answers f opts =
  let fun helper xs =
        case xs of
             [] => []
           | x::xs' => case (f x) of
                            SOME y => y @ (helper xs')
                          | _ => raise NoAnswer
  in SOME(helper opts) handle NoAnswer => NONE end

(* Problem Num 9 *)
(* common helper methods, spelled out for humans *)
(* function (fn x=>1) always returns 1 *)
(* function (fn x=>0) always returns 0 *)
val add1 = (fn x=>1)
val add0 = (fn x=>0)

val count_wildcards = g add1 add0

fun count_wild_and_variable_lengths p =
  (count_wildcards p) + (g add0 String.size p)

fun count_some_var (s,p) = g add0 (fn x => if(s=x) then 1 else 0) p

(* Problem Num 10 *)
fun check_pat p =
  let
    fun get_name pp =
      case pp of Variable x => [x]
         | ConstructorP(_,pp') => (get_name pp')
         | TupleP ppl => List.foldl (fn(pp',ac) => (get_name pp') @ ac) [] ppl
         | _ => []
    fun vars_unique lst =
      case lst of [] => true
         | x::lst' => (List.foldl(fn(y,acc)=>(acc andalso not(x=y)))
                                  true lst') andalso (vars_unique lst')
  in
    vars_unique (get_name p)
  end

(* Problem Num 11 *)
fun match (v,p) =
  case (p,v) of
       (Wildcard,_) => SOME[]
     | (Variable s,_) => SOME[(s,v)]
     | (UnitP,Unit) => SOME[]
     | (ConstP cp,Const cv) => SOME[]
     | (ConstructorP(s1,cp), Constructor(s2,vv)) =>
         if (s1=s2) then match(vv,cp) else NONE
     | (TupleP tp, Tuple tv) =>
         if (List.length(tp)=List.length(tv))
         then all_answers match (ListPair.zip(tv,tp))
         else NONE
     | _ => NONE

(* Problem Num 12 *)
fun first_match (v,pl) =
  let fun curry_match x y = match(x,y) in
    SOME(first_answer (curry_match v) pl) handle NoAnswer => NONE
  end

