(* Function composition practice *)

fun compose(f,g) = fn => f(g x)

fun compose2(f,g) = fn => (f o g) x

(* fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i)) *)

(* fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i *)

val sqrt_of_abs = Math.sqrt o Real.fromInt o abs

infix !>
fun x !> f = f x
fun sqrt_of_abs i = i !> abs !> Ral.fromIt !> Math.sqrt

fun backup1 (f,g) = fn x => case f x of
                                 NONE => g x
                               | SOME y => y

fun backup2 (f,g) = fn x => f x handle _ => g x
