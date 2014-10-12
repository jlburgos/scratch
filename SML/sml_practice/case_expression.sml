(* Practicing Case Expressions *)

use "./datatype.sml";

(* could have written -- fun f (x : mytype) -- *)
fun f x =
  case x of
       Pizza => 3
     | Str s => 8
     | TwoInts(i1,i2) => i1 + i2

val x = f (Str "hi")

fun ff yy = case of yy
  Pizza => 2 | Str s => 2
               | TwoInts(x,y) => 3
