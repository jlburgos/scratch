(* Practicing with Map and Filter ... famous higher-order functions *)

fun map(f,xs) =
  case xs of
       [] => []
     | x::xs' => (f x)::map(f,xs')

val x1 = map((fn x => x+1), [4,8,12,16])
val x2 = map(hd, [[42,3,4],[3,23,4],[1,1,1]])

fun filter(f,xs) =
  case xs of
       [] => []
     | x::xs' =>  if (f x) then x::(filter(f,xs'))
                  else filter(f,xs')

fun is_even v =
  (v mod 2 = 0)

fun all_even xs = filter(is_even, xs)

(* If second item in a pair is an even int, accept *)
fun all_even_snd xs = filter((fn (_,v) => is_even v), xs)


