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


(* Problem Num 8 *)
fun all_answers f opts =
  let fun helper xs =
        case xs of
             [] => []
           | x::xs' => case (f x) of
                            SOME y => y @ (helper xs')
                          | _ => raise NoAnswer
  in SOME(helper opts) handle NoAnswer => NONE end

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
         then all_answers(match,ListPair.zip(tv,tp))
         else NONE
     | _ => NONE

