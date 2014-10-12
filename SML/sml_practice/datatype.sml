(* Practicing with datatypes *)

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza


val x = TwoInts (3,5)
val y = Str
val yy = Str "hi"
val z = Pizza

