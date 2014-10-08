(* Practicing with partial applications *)

val sorted3 = fn x => fn y => fn z => z >=y andalso y >= x
fun fold f acc xs =
  case xs of
       [] => acc
     | x::xs' => fold f (f(acc,x)) xs'

(* fold takes 3 args... the sum binding below creates a sum function *)
val sum = fold (fn(x,y) => x+y) 0
val total = sum [1,2,3,4]

val is_nonnegative = sorted3 0 0

(* inferior ways : they do function wrapping *)
fun is_nonnegative_inferior x = sorted3 0 0 x
fun sum_inferior xs = fold (fn(x,y) => x+y) 0 xs

(* another example *)
fun range i j = if i > j then [] else i :: range (i+1) j
val countup = range 1
fun countup_inferior x = range 1 x (* function wrapping *)

fun exists predicate xs =
  case xs of
       [] => false
     | x::xs' => predicate x orelse exists predicate xs'

(* simple example *)
val no exists (fn x => x-7) [4,11,23]

(* int list -> bool *)
val hasZero = exists (fn x=>x=0)

(* int list -> int list ... adds 1 to every element in the int list *)
val incrementAll = List.map (fn x => x+1)

(* int list -> int list ... remove zeroes *)
val removeZeroes = List.filter (fn x => x <> 0)

(* Sometimes we will need function wrapping in polymorphic partial apps *)
(* val pairWithOne = List.map (fn x => (x,1)) ... will not work *)
fun pairWithOne xs = List.map (fn x=> (x,1)) xs

(* taking a function and currying it *)
(* fun curry f x y = fn x => fn y => f(x,y) *)
fun curry f x y = f(x,y)
(* val countUp2 = (curry range) 1 *)
val countUp2 = curry range 1

(* uncurry method *)
fun uncurry f(x,y) = f x y

(* switch args *)
(* fun other_curry = fn x => fn y => f y x *)
fun other_curry = f y x


