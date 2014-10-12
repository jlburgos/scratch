(* Homework 2 Code *)

(* Author - SlicedBread *)
(* Original helper methods by - Dr. Dan Grossman *)


(*************************************************)
(*************************************************)
(*************************************************)
(*  Problem 1 *)

fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put solutions for problem 1 here *)

fun all_except_option(s : string, lst : string list) =
  let
    fun remove_str(lst' : string list) =
      case lst' of
          [] => []
        | s'::lst'' => if same_string(s,s') then lst''
                       else s'::remove_str(lst'')
    val res = remove_str(lst)
  in
    if (res = lst) then NONE
    else SOME(res)
  end
(*************************************************)
fun get_substitutions1(sll : string list list, s : string) =
  case sll of
       [] => []
     |sl::sll' => let val sl' = all_except_option(s,sl) in
                      case sl' of
                           NONE => get_substitutions1(sll',s)
                         | SOME v => v @ get_substitutions1(sll',s)
                      end
(*************************************************)
fun get_substitutions2(sll : string list list, s : string) =
  let
    fun tail_recurse(sll' : string list list, newlist : string list) =
      case sll' of
           [] => newlist
         |sl::sll'' => let val sl' = all_except_option(s,sl) in
                          case sl' of
                               NONE => tail_recurse(sll'', newlist)
                             | SOME v => tail_recurse(sll'', newlist @ v)
                        end
  in
    tail_recurse(sll, [])
  end
(*************************************************)
type FullName = { first  : string,
                  middle : string,
                  last   : string }
fun similar_names(sll : string list list, name : FullName) =
  let
    val {first=f,middle=m,last=l} = name
    fun helper(subs : string list) =
      case subs of
           [] => []
         | s::subs' => {first=s,middle=m,last=l} :: helper(subs')
  in
    name::helper(get_substitutions2(sll,f))
  end
(*************************************************)


(*************************************************)
(*************************************************)
(*************************************************)
(*  Problem 2 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put solutions for problem 2 here *)

fun card_color((s,r) : card) =
  case s of
       Diamonds => Red
     | Hearts => Red
     | _ => Black
(*************************************************)

fun card_value((s,r) : card) =
  case r of
       Jack => 10 | Queen => 10 | King => 10
     | Ace => 11
     | Num v => v
(*************************************************)
fun remove_card(cs : card list, c : card, e) =
  let
    fun helper(cs' : card list) =
      case cs' of
           [] => []
         | c'::cs'' => if c=c' then cs'' else c'::helper(cs'')
    val cs' = helper(cs)
  in
    if (cs=cs') then raise e
    else cs'
  end
(*************************************************)
fun all_same_color(cs : card list) =
  case cs of
       [] => true
     | c1::[] => true
     | c1::c2::cs' => if card_color(c1)=card_color(c2)
                      then all_same_color(cs')
                      else false
(*************************************************)
fun sum_cards(cs : card list) =
  let
    fun tail_recurse(cs' : card list, sum : int) =
      case cs' of
           [] => sum
         | c::cs'' => tail_recurse(cs'',sum+card_value(c))
  in
    tail_recurse(cs,0)
  end
(*************************************************)
fun score(cs : card list, goal : int) =
  let
    val sum = sum_cards(cs)
    val final = if sum>goal then 3*(sum-goal) else (goal-sum)
  in
    case all_same_color(cs) of
         false => final
       | _ => (final div 2)
  end
(*************************************************)
fun officiate(deck: card list, moves: move list, goal: int) =
  let
    fun draw(stack : card list ) =
      case stack of
           [] => NONE
         | c::stack' => SOME c
    fun play(hand: card list, stack: card list, mvs: move list) =
      if sum_cards(hand)>goal then score(hand,goal)
      else
        case mvs of
             [] => score(hand, goal)
           | m::mvs' => case m of
                Discard c => play(remove_card(hand,c,IllegalMove),stack,mvs')
              | Draw => case draw(stack) of
                    NONE => score(hand, goal)
                  | SOME nc =>
                      play(nc::hand,remove_card(stack,nc,IllegalMove),mvs')

  in
    play([],deck,moves)
  end
(*************************************************)

