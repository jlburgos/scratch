(* Practice Currying *)

(* old way *)
fun sorted3_tupled (x,y,z) = z >= y andalso y >= x

val t1 = sorted3_tupled (7,9,11)

(* New way *)
val sorted3 = fn x => fn y => fn z => z >=y andalso y >= x

val t2 = sorted3 7 9 11
val t3 = ((sorted3 7) 9) 11

(*
val wrong1 = sorted3_typled 7 9 11
val wrong2 = sorted3 (7,9,11)
*)

(* syntactic sugar for currying, same as sorted3() *)
fun sorted3_nicer x y z = z >= y andalso y >= x

val t4 = sorted3_nicer 7 9 11
val t5 = ((sorted3_nicer 7) 9) 11

(* a more useful example *)
fun fold f acc xs = (* means fun fold = fn acc => fn xs => .... *)
  case xs of
       [] => acc
     | x::xs' => fold f (f(acc,x)) xs' (* need (..) to let f know it's args *)

fun sum xs = fold (fn (x,y) => x+y) 0 xs

(*
fun myfoldr f acc xs =
  case xs of
       [] => acc
     | x::xs' => fold f (f(acc,x)) xs' (* need (..) to let f know it's args *)
fun myfoldl f acc xs =
  case xs of
       [] => acc
     | x::xs' => fold f (f(acc,x)) xs' (* need (..) to let f know it's args *)
*)



