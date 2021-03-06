(***************************
 * Assignment #1
 * Author: Juan Burgos
 ***************************)


(*************************************************)
(*  Problem #1: is_older *)
(*
 *  Compares two dates evaluating to true if the first is strictly
 *  older than the second and false otherwise.
 *
 *  @param day1 :: (int * int * int)
 *    - First date.
 *  @param day2 :: (int * int * int)
 *    - Second date.
 *  @return :: boolean
 *    - True or false.
 *)
fun is_older(day1 : (int * int * int), day2 : (int * int * int)) =
  let
    val y1 = (#1 day1)
    val y2 = (#1 day2)
    val m1 = (#2 day1)
    val m2 = (#2 day2)
    val d1 = (#3 day1)
    val d2 = (#3 day2)
  in
    (y1 < y2) orelse
    (y1 = y2 andalso m1 < m2) orelse
    (y1 = y2 andalso m1 = m2 andalso d1 < d2)
  end
(*************************************************)


(*************************************************)
(*  Problem #2: number_in_month *)
(*
 *  Counts the number of the dates in the list that are contained
 *  within the given month.
 *
 *  @param dates :: (int * int * int) list
 *    - List of dates to test.
 *  @param month :: int
 *    - Numeric value for the given month.
 *  @return :: int
 *    - Count of how many dates were contained in the given month.
 *
 *)
fun number_in_month(dates : (int * int * int) list, month : int) =
  if null(dates) then 0
  else
    let
      fun collect_dates(dd : (int * int * int) list) =
        let
          val test = if ((#2 (hd dd))=month) then 1
                     else 0
        in
          if null(tl dd) then test
          else test + collect_dates(tl dd)
        end
    in
      collect_dates(dates)
    end
(*************************************************)


(*************************************************)
(*  Problem #3: number_in_months *)
(*
 *  Counts the number of the dates in the list that are contained
 *  within the given list of months.
 *
 *  @param dates :: (int * int * int) list
 *    - List of dates to test.
 *  @param month :: int list
 *    - List of numeric month values.
 *  @return :: int
 *    - Count of how many dates were contained in the given month.
 *
 *)
fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null(dates) orelse null(months) then 0
  else
    let
      fun collect_dates(mm : int list) =
        if null(tl mm) then number_in_month(dates, (hd mm))
        else
          number_in_month(dates, (hd mm)) + collect_dates(tl mm)
    in
      collect_dates(months)
    end
(*************************************************)


(*************************************************)
(*  Problem #4: dates_in_month *)
(*
 *  Collect dates that are present in the given month.
 *
 *  @param dates :: (int * int * int) list
 *    - List of calendar dates (year, month, day).
 *  @param month :: int
 *    - Numeric value for a month.
 *  @return :: (int * int * int) list
 *    - List of calendar dates within the given month (maintained original.
 *    ordering)
 *
 *)
fun dates_in_month(dates : (int * int * int) list, month : int) =
  let
    fun collect_dates(dates : (int * int * int) list) =
      if      null(dates) then []
      else if ((#2 (hd dates))=month) then 
              (hd dates) :: collect_dates(tl dates)
      else    collect_dates(tl dates)
  in
    collect_dates(dates)
  end
(*************************************************)


(*************************************************)
(*  Problem #5: dates_in_months *)
(*
 *  Collects dates present in any of the given months.
 *
 *  @param dates :: (int * int * int) list
 *    - List of calendar dates (year, month, day).
 *  @param months :: int list
 *    - List of numeric values for months.
 *  @return :: (int * int * int) list
 *    - Dates contained in any of the given months.
 *
 *)
fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null(dates) orelse null(months) then []
  else
    let
      fun dates_in_months_nonempty(mm : int list) =
        if null(tl mm) then dates_in_month(dates, hd mm)
        else
          dates_in_month(dates, hd mm) @ dates_in_months_nonempty(tl mm)
    in
      dates_in_months_nonempty(months)
    end
(*************************************************)


(*************************************************)
(*  Problem #6: get_nth *)
(*
 *  Return the nth string in a list of strings.
 *
 *  @param ss :: string list
 *    - List of strings.
 *  @param n :: int
 *    - Number of element to be returned.
 *  @return :: string
 *    - Chosen string to be returned.
 *
 *  @assumption
 *    - We assume that (length(ss) >= n > 0) is true for n,
 *      since the problem statement allows for this assumption.
 *
 *)
fun get_nth(ss : string list, n : int) =
  if n=1 then (hd ss)
  else get_nth(tl(ss), n-1)
(*************************************************)


(*************************************************)
(*  Problem #7: date_to_string *)
(*
 *  Return a string version of a date originally given as
 *  (year, month, day) ... all integers.
 *
 *  @param date :: (int * int * int)
 *    - A calendar date of the form: (year, month, day).
 *  @return :: string list
 *    - Stringified version of the input calendar date.
 *
 *)
fun date_to_string(dates : (int * int * int)) =
  let
    val months = ["January","February","March","April",
                  "May","June","July","August","September",
                  "October","November","December"]
    val day   = Int.toString(#3 dates)
    val month = get_nth(months, #2 dates)
    val year  = Int.toString(#1 dates)
  in
    month ^ " " ^ day ^ ", " ^ year
  end
(*************************************************)


(*************************************************)
(*  Problem #8: number_before_reaching_sum *)
(*
 *  Returns the count of the first "n" integers whose sum
 *  is strictly less than the "sum" argument.
 *
 *  @param sum :: int
 *    - Exclusive upper limit of this method's output.
 *  @param values :: int list
 *    - List of integers for conducting the partial sum.
 *  @return :: int
 *    - The location of the nth int where the sum(1...n) < sum
 *      but sum(1...n+1) >= sum.
 *
 *  @assumption
 *    - Assumes the value "sum" is positive and is not larger than
 *      the sum of all integers in the values int list.
 *)
fun number_before_reaching_sum(sum : int, values : int list) =
  let
    fun partial_sum_count(v : int, ll : int list) =
      if v+(hd ll) >= sum then 0
      else 1+partial_sum_count(v+hd(ll), tl(ll))
  in
    partial_sum_count(0, values)
  end
(*************************************************)


(*************************************************)
(*  Problem #9: what_month *)
(*
 *  Given a day of the year between 1-365, return that day's month.
 *
 *  @param day :: int
 *    - A day between 1-365.
 *  @return :: int
 *    - Numeric value for the month that contains the day.
 *
 *  @assumption
 *    - Ignoring leap years.
 *)
fun what_month(day : int) =
  let
    val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    1+number_before_reaching_sum(day,days_in_months)
  end
(*************************************************)

(*************************************************)
(*  Problem #10: month_range *)
(*
 *  Given a pair of dates, we return the inclusive range of
 *  of months between them.
 *
 *  @param d1 :: int
 *    - First day in the range calculation.
 *  @param d2 :: int
 *    - Second day in the range calculation.
 *  @return :: string list
 *    - Inclusive range of months between the two dates.
 *)
fun month_range(d1 : int, d2 : int) =
  if d1 > d2 then []
  else what_month(d1) :: month_range(d1+1, d2)
(*************************************************)


(*************************************************)
(*  Problem #11: oldest *)
(*
 *  Returns the oldest date in the given list.
 *
 *  @param dates :: (int * int * int) list
 *    - List of calendar dates to compare.
 *  @return :: (int * int * int) option
 *    - If any dates are given, we return SOME (int * int * int)
 *      corresponding to the oldest date, otherwise we
 *      return NONE.
 *
 *)
fun oldest(dates : (int * int * int) list) =
  if null(dates) then NONE
  else
    let
      fun oldest_date(days : (int * int * int) list) =
        if null(tl days) then (hd days)
        else
          let
            val day = oldest_date(tl days)
          in
            if is_older(hd(days), day) then (hd days)
            else day
          end
    in
      SOME (oldest_date dates)
    end
(*************************************************)


(*************************************************)
(************* CHALLENGE PROBLEMS ****************)
(*************************************************)

(*************************************************)
(*  Problem #12a-12b (HELPER METHOD) : duplicate_free_copy *)
(*
 *  Returns a duplicate-free copy of the original int list by
 *  rebuilding the int list backwards while avoiding repeated
 *  values.
 *
 *  @param numbers :: int list
 *    - List of integers.
 *  @return :: int list
 *    - Duplicate-free version of the input.
 *
 *  @notes
 *    - This method is used for problems #12a and #12b.
 *)
fun duplicate_free_copy(numbers : int list) =
  if null(numbers) then []
  else
    let
      fun duplicate_free_copy_nonempty(nums : int list) =
        if null(tl nums) then [(hd nums)]
        else
          let
            (* Call top level method so we only deal with non-empty lists *)
            val non_dup_nums = duplicate_free_copy(tl nums)
            val new_num = (hd nums)
            (* Find the first duplicate int given non_dup_nums:
             *   If a duplicate is found, return true immediately.
             *   Otherwise, keep searching until the end of the list. *)
            fun find_duplicate_in(nn : int list) =
              if null(tl nn) then (new_num=(hd nn))
              else
                if (new_num=(hd nn)) then true
                else find_duplicate_in(tl nn)
          in
            (* Only add the new integer if a dup was not found *)
            if find_duplicate_in(non_dup_nums) then non_dup_nums
            else new_num :: non_dup_nums
          end
    in
      duplicate_free_copy_nonempty(numbers)
    end
(*************************************************)


(*************************************************)
(*  Problem #12a: number_in_months_challenge *)
(*
 *  Wrapper around the number_in_months() method
 *  that removes duplicate months.
 *
 *  @param dates :: (int * int * int) list
 *    - List of input dates.
 *  @param months :: int list
 *    - List of input months.
 *  @return :: int
 *    - Number of days in dates contained in any
 *      of the input months.
 *
 *)
fun number_in_months_challenge( dates : (int * int * int) list,
                                months : int list) =
  if null(months) then 0
  else number_in_months(dates, duplicate_free_copy(months))
(*************************************************)


(*************************************************)
(*  Problem #12b: dates_in_months_challenge *)
(*
 *  Wrapper around the dates_in_months() method
 *  that removes duplicate months.
 *
 *  @param dates :: (int * int * int) list
 *    - Input list of dates.
 *  @param months :: int list
 *    - Input list of months.
 *  @return :: (int * int * int) list
 *    - List of dates contained in any of the
 *      input months.
 *
 *)
fun dates_in_months_challenge(dates : (int * int * int) list, 
                              months : int list) =
  if null(months) then []
  else dates_in_months(dates, duplicate_free_copy(months))

(*************************************************)


(*************************************************)
(*  Problem #13: reasonable_date *)
(*
 *  Decides if the given date is reasonable given
 *  the condition that the year be non-negative,
 *  month be between 1-12 and the day correspond to
 *  a day that is possible in the given month and
 *  year (taking leap-years into account).
 *
 *  @param date :: (int * int * int)
 *    - The input date to be checked.
 *  @return :: boolean
 *    - True if the date is reasonable, false
 *      otherwise.
 *
 *)
fun reasonable_date(date : (int * int * int)) =
  let
    val y = (#1 date) (* year *)
    val m = (#2 date) (* month *)
    val d = (#3 date) (* day *)

    val test_year = (y > 0)
    val test_month = (m > 0) andalso (m <= 12)
    val test_day =
      let
        val maxday =
          if (m=2) then
            let
              val is_leap = (Int.mod(y,400)=0) orelse
                            ((Int.mod(y,4)=0) andalso not (Int.mod(y,100)=0))
            in
              if is_leap then 29
              else 28
            end
          else if (m=4 orelse m=6 orelse m=9 orelse m=11) then 30
          else 31
      in
        (d > 0) andalso (d <= maxday)
      end
  in
    test_year andalso test_month andalso test_day
  end
(*************************************************)

