(*Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.) *)
(* year month day *)
fun is_older(date1: (int*int*int), date2: (int*int*int))=
  let val y1 = #1(date1);
      val m1 = #2(date1);
      val d1 = #3(date1);
      val y2 = #1(date2);
      val m2 = #2(date2);
      val d2 = #3(date2);
  in
    if y1<y2 then
      true
    else if y1 = y2 andalso  m1 < m2 then
      true
    else if  y1 = y2 andalso m1 = m2 andalso d1 < d2 then
      true
    else
     false
  end

(* tests *)

val test1 = is_older ((1,2,3),(2,3,4)) = true;
val test2 = is_older((2001,10,1),(2001,11,1)) = true;
val test3 = is_older((2001,10,1),(2001,10,1)) = false;
val test4 = is_older((2001,10,1),(2001,10,2)) = true;
val test5 = is_older((2001,8,31),(2001,10,1)) =true;

(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month *)

fun number_in_month(dates: (int * int * int) list, month: int)=
  if null(dates) then
    0
  else
    if (#2(hd (dates)) = month) then
      1+number_in_month(tl(dates),month)
    else
      0+number_in_month(tl(dates),month)

val test6 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;
val test7 = number_in_month ([(2012,0,30),(2012,0,30)],5) = 0;

fun number_in_months(dates: (int * int * int) list, months: int list)=
  if null (months) then
    0
  else
    number_in_month(dates, hd (months)) + number_in_months(dates,tl(months))

val test8 =  number_in_months  ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test9 =  number_in_months  ([(2012,2,28),(2013,7,1),(2071,3,31),(2011,3,28)],[2,3,4]) = 3
val test10 = number_in_months ([(2012,1,28),(2013,7,1),(2071,8,31),(2011,12,28)],[1,3,4]) = 1
val test11 = number_in_months ([(2012,1,28),(2013,7,1),(2071,8,31),(2011,12,28)],[1]) = 1
val test12 = number_in_months ([(2012,1,28),(2013,7,1),(2071,8,31),(2011,12,28)],[1]) = 1
val test13 = number_in_months ([(2012,1,28)],[1,28]) = 1

fun dates_in_month(dates: (int * int * int) list, month: int)=
    if null (dates) then
      []
    else
       if (#2(hd (dates)) = month) then
         hd(dates) :: dates_in_month(tl(dates),month)
       else
         dates_in_month(tl(dates),month)

val test14 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test15 = dates_in_month ([(2012,12,28),(2013,2,1),(2017,6,2),(2014,2,28)],2) = [(2013,2,1),(2014,2,28)]

fun dates_in_months(dates: (int * int * int) list, months: int list)=
  if null (months) then
     []
  else
    dates_in_month(dates,hd(months)) @ dates_in_months(dates,tl(months))


val test16 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test17 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,4]) = [(2012,2,28),(2011,4,28)]

fun  get_nth(words: string list, nth: int)=
  let
    fun count_list_elements(words: string list, current_index: int)=
      if current_index = nth then
        hd(words)
      else
        count_list_elements(tl(words),current_index+1)
  in
    count_list_elements(words,1)
  end

val test18 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test19 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test20 = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"

fun date_to_string(date: (int * int * int))=
 let val month_string = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
 in
   get_nth(month_string,#2(date)) ^ " " ^ Int.toString(#3(date)) ^ ", " ^ Int.toString(#1(date))
 end

 val test20 = date_to_string (2013, 6, 1) = "June 1, 2013"
 val test21 = date_to_string (2013, 1, 5) = "January 5, 2013"
 val test22 = date_to_string (2013,12, 5) = "December 5, 2013"

fun number_before_reaching_sum(sum: int, numbers: int list)=
  if (sum - hd(numbers)) <=0 then
    0
  else
    1+ number_before_reaching_sum((sum-hd(numbers),tl(numbers)))


val test23 = number_before_reaching_sum (10, [1,2,3,4,5])=3;
val test24 = number_before_reaching_sum (15, [30,2,3,4,5])=0;
val test25 = number_before_reaching_sum (31, [30,2,3,4,5])=1;

fun what_month(day: int)=
  let val month_days = [31,28,31,30,31,30,31,31,30,31,30,31];
  in
       1+number_before_reaching_sum(day,month_days)
  end

val test26 = what_month (70) = 3;


fun month_range(day1: int, day2: int)=
  if (day1>day2) then
    []
  else
    let
      fun inner(from:int, to:int)=
        if (from=to) then
          [what_month(from)]
        else
          what_month(from) :: inner(from+1,to)
    in
      inner(day1,day2)
    end

val test27 = month_range (31, 34)= [1,2,2,2];

fun oldest(dates: (int * int * int ) list)=
  if null (dates) then
    NONE
  else if null (tl dates) then
    SOME(hd dates)
  else
    let val tl_ans = oldest(tl(dates))
    in
      if isSome(tl_ans) andalso is_older(hd(dates), valOf(tl_ans)) then
        SOME(hd(dates))
      else
        tl_ans
    end

val test28 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]);
val test29 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

