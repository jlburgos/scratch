(***************************
 * Assignment #1 : TEST FILE
 * Author: Juan Burgos
 ***************************)

use "assignment1.sml";

(* Provided tests from the coursera webpage *)

val test1 = is_older((1,2,3),(2,3,4)) = true

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1
val test3 = number_in_months([(2012,2,28),(2013,12,1),
                              (2011,3,31),(3011,4,28)],[2,3,4]) = 3
val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test5 =  dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),
                              (2011,4,28)],[2,3,4]) =
                            [(2012,2,28),(2011,3,31),(2011,4,28)]
val test6 = get_nth(["hi","there","how","are","you"],2) = "there"
val test7 = date_to_string((2013,6,1)) = "June 1, 2013"
val test8 = number_before_reaching_sum(10,[1,2,3,4,5]) = 3
val test9 = what_month(70) = 3
val test10 = month_range(31,34) = [1,2,2,2]
val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

(* My own test *)

val test1b = is_older((2,2,3),(2,1,4)) = false
val test8b = number_before_reaching_sum(10,[1,2,3,4,5]) = 3
val test8c = number_before_reaching_sum(21,[5,2,6,3,2,6]) = 5
val test8d = number_before_reaching_sum(1,[2,3,4]) = 0
val test8e = number_before_reaching_sum(3,[2,3,4]) = 1
val test9b = what_month(70) = 3
val test9c = what_month(29) = 1
val test9d = what_month(32) = 2
val test10b = month_range(29,32) = [1,1,1,2]
val test11b = oldest([(2011,2,28),(2011,1,28),(2011,4,28)]) = SOME (2011,1,28)

(* Testing the Challenge Problems *)

val test12a1 = number_in_months([(2012,2,28),(2013,12,1),
                              (2011,3,31),(3011,4,28)],[2,3,3,3,4]) = 5
val test12a2 = number_in_months_challenge([(2012,2,28),(2013,12,1),
                              (2011,3,31),(3011,4,28)],[3,3,3,3,3]) = 1
val test12a3 = number_in_months_challenge([(2012,2,28),(2013,12,1),
                              (2011,3,31),(3011,4,28)],[]) = 0
val test12b1 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),
                              (2011,4,28)],[2,3,3,3,4]) =
                            [(2012,2,28),(2011,3,31),(2011,3,31),
                              (2011,3,31),(2011,4,28)]
val test12b2 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),
                              (2011,4,28)],[2,3,3,3,4]) =
                            [(2012,2,28),(2011,3,31),(2011,4,28)]
