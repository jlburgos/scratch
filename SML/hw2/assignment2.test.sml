(* Test cases for assignment #2 *)

use "assignment2.sml";

(* Problem 1a *)
val t1a1 = NONE = all_except_option("yes",[])
val t1a2 = NONE = all_except_option("yes",["no"])
val t1a3 = SOME["no"] = all_except_option("yes",["yes","no"])
val t1a4 = SOME["no"] = all_except_option("yes",["no","yes"])
val t1a5 = SOME["no","maybe"] = all_except_option("yes",["no","yes","maybe"])

(* Problem 1b *)
val t1b1 = ["Fredrick","Freddie","F"] =
  get_substitutions1([["Fred","Fredrick"],
                      ["Elizabeth","Betty"],
                      ["Freddie","Fred","F"]],
                      "Fred")
val t1b2 = ["Jeffrey","Geoff","Jeffrey"] =
  get_substitutions1([["Fred","Fredrick"],
                      ["Jeff","Jeffrey"],
                      ["Geoff","Jeff","Jeffrey"]],
                      "Jeff")

(* Problem 1c *)
val t1c1 = ["Fredrick","Freddie","F","Fernando","Fredrick"] =
  get_substitutions2([["Fred","Fredrick"],
                      ["Elizabeth","Betty"],
                      ["Freddie","Fred","F"],
                      ["Fred","Fernando"],
                      ["Fred","Fredrick"]],
                      "Fred")
val t1c2 = ["Fernando","Jeffrey","Geoff","Jeffrey"] =
  get_substitutions2([["Jeff","Fernando"],
                      ["Fred","Fredrick"],
                      ["Jeff","Jeffrey"],
                      ["Geoff","Jeff","Jeffrey"]],
                      "Jeff")

(* Problem 1d *)
val myName : FullName = {first = "Juan", middle = "Luis", last = "Burgos"}
val t1d1 = similar_names([[]], myName) = [myName]

(********************************************************************)
(********************************************************************)

(* Problem 2a *)
val t2a1 = card_color((Hearts, Num 2)) = Red

(* Problem 2b *)
val t2b1 = card_value((Clubs, Num 2)) = 2
val t2b2 = card_value((Diamonds, Queen)) = 10
val t2b3 = card_value((Spades, Ace)) = 11

(* Problem 2c *)
val t2c1 = (remove_card([(Spades, Ace)],
                        (Hearts, Ace),
                        IllegalMove) = [(Spades,Ace)] handle IllegalMove =>
                        true)
val t2c2 = remove_card([(Hearts, Num 4),(Hearts, Ace),(Spades, Ace)],
                        (Hearts, Ace),
                        IllegalMove) = [(Hearts, Num 4), (Spades, Ace)]
val t2c3 = remove_card([(Hearts, Ace),(Hearts,Ace)],
                        (Hearts, Ace),
                        IllegalMove) = [(Hearts, Ace)]

(* Problem 2d *)
val t2d1 = all_same_color([(Hearts, Ace), (Hearts, Queen)]) = true
val t2d2 = all_same_color([(Hearts, Ace), (Diamonds, Queen)]) = true
val t2d3 = all_same_color([(Hearts, Ace), (Spades, Queen)]) = false
val t2d4 = all_same_color([(Spades, Queen)]) = true
val t2d5 = all_same_color([]) = true
val t2d6 = all_same_color([(Hearts, Ace), (Clubs, Num 3)]) = false

(* Problem 2e *)
val t2e1 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val t2e2 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val t2e3 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val t2e4 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val t2e5 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

(* Problem 2f *)
val t2f1 = score([(Hearts, Num 2),(Clubs, Num 4)],10)=4
val t2f2 = score([(Hearts, Num 2),(Diamonds, Num 3)],10)=2
val t2f3 = score([(Hearts, Num 2),(Diamonds, Num 2)],10)=3
val t2f4 = score([(Hearts, Num 2),(Diamonds, Ace)],10)=4
val t2f5 = score([(Hearts, Num 2),(Spades, Ace)],10)=9

(* Problem 2g *)
val t2g1 = officiate([(Hearts, Num 2),(Clubs, Ace)],[Draw], 15) = 6

val t2g2 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Discard(Clubs,Ace),Draw,Draw,
                       Discard(Clubs,Ace)], 5) = 9

(********************************************************************)
(********************************************************************)
(* Homework2 Simple Tests - hw2test.sml *)
(********************************************************************)
(********************************************************************)

(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option("string", ["string"]) = SOME []

val test2 = get_substitutions1([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2([["foo"],["there"]], "foo") = []

val test4 =
    similar_names([ ["Fred","Fredrick"],
                    ["Elizabeth","Betty"],
                    ["Freddie","Fred","F"]],
                  {first="Fred",middle="W", last="Smith"})  =
	      [ {first="Fred"     , last="Smith", middle="W"},
          {first="Fredrick" , last="Smith", middle="W"},
	        {first="Freddie"  , last="Smith", middle="W"},
          {first="F"        , last="Smith", middle="W"} ]

val test5 = card_color((Clubs, Num 2)) = Black

val test6 = card_value((Clubs, Num 2)) = 2

val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true

val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)

