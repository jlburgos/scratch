(* Practicing with anonymous functions *)

fun n_times (f,n,x) =
  if n=0
  then x
  else f(n_times(f,n-1,x))

fun triple_n_times(n,x) =
  n_times((fn y => 3*y), n, x)

val triple = fn y => 3*y

(* Instead of doing unnecessary function mapping *)
fun rev xs = List.rev xs

(* Which translates to the following w/o syntactic sugar *)
val rev = fn xs => List.rev xs

(* Do the following expression *)
val rev = List.rev
(* This defines the first class function rev *)
