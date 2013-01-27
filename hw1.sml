(* takes two dates and evaluates to true or false. It evaluates to true if the first 
argument is a date that comes before the second argument. (If the two dates are the 
same, the result is false.) *)
fun is_older (d_1 : (int * int * int), d_2: (int * int * int)) = 
    let
	val y1 = #1 d_1
	val y2 = #1 d_2
	val m1 = #2 d_1
	val m2 = #2 d_2
	val d1 = #3 d_1
	val d2 = #3 d_2
    in
	not (y1 >= y2 andalso m1 >= m2 andalso d1 >= d2)
    end

(* takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month *)
fun number_in_month (dates : (int * int * int) list, month : int) = 
    if null dates
    then 0
    else
	let 
	    val m = #2 (hd date)
	    val cnt = if m = month then 1 else 0		     
	in
	    cnt + number_in_month((tl dates), month)	    
	end

(* takes a list of dates and a list of months (i.e., an int list) 
and returns the number of dates in the list of dates that are in 
any of the months in the list of months. Assume the list of months 
has no number repeated. Hint: Use your answer to the previous problem*)
fun number_in_months (dates : (int * int * int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

(* takes a list of dates and a month (i.e., an int) and returns a list 
holding the dates from the argument list of dates that are in the month. 
The returned list should contain dates in the order they were originally given *)
fun dates_in_month (dates : (int * int * int) list, month : int) = 
    if null dates
    then []
    else
	let 
	    val date = hd dates
	    val tl_dates = tl dates
	in
	    if #2 date = month
	    then date :: dates_in_month(tl_dates, month)
	    else dates_in_month(tl_dates, month)
	end

(* takes a list of dates and a list of months (i.e., an int list) and returns a 
list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your 
answer to the previous problem and SML's list-append operator (@).*)
fun dates_in_months (dates : (int * int * int) list, months : int list) = 
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

(* takes a list of strings and an int n and returns the nth element of the list 
where the head of the list is 1st. Do not worry about the case where the list has 
too few elements: your function may apply hd or tl to the empty list in this case, 
which is okay.*)
fun get_nth (strings : string list, n : int) = 
    if n = 1
    then hd strings
    else get_nth((tl strings), n-1)

(* takes a date and returns a string of the form January 20, 2013 (for example). 
Use the operator ^ for concatenating strings and the library function Int.toString for 
converting an int to a string. For producing the month part, do not use a bunch of 
conditionals.Instead, use a list holding 12 strings and your answer to the previous 
problem. For consistency, put a comma following the day and use capitalized English 
month names: January, February, March, April,May, June, July, August, September, October, 
November, December.*)
fun date_to_string (date : int * int * int) = 
    let 
	val month_string_list = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month_string_list, (#2 date)) ^ 
	" " ^ 
	(Int.toString((#3 date))) ^ 
	", " ^ 
	(Int.toString((#1 date)))
    end

(* takes an int called sum, which you can assume is positive, and an int list, which 
you can assume contains all positive numbers, and returns an int. You should return an 
int n such that the rst n elements of the list add to less than sum, but the rst
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than 
the passed in value; it is okay for an exception to occur if this is not the case.*)
fun number_before_reaching_sum (sum : int, ints : int list) = 
    if hd ints >= sum
    then 0
    else 1 + number_before_reaching_sum(sum-first, (tl ints))

(* takes a day of year (i.e., an int between 1 and 365) and returns what month that day 
is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.*)
fun what_month (day : int) =     
    let 
	val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, days_of_months)
    end

(* takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] 
where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.*)
fun month_range (day1 : int, day2 : int) =     
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(* takes a list of dates and evaluates to an (int*int*int) option. It evaluates to 
NONE if the list has no dates and SOME d if the date d is the oldest date in the list.*)
fun oldest (dates : (int * int * int) list) = 
    if null dates
    then NONE
    else
	let
	    val oldest_rest = oldest((tl dates))
	    val current = hd dates
	in
	    if isSome oldest_rest andalso is_older((valOf(oldest_rest)), current)
	    then oldest_rest
	    else SOME current
	end	    

(**********************)
(* CHALLENGE PROBLEMS *)
(**********************)
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    let
	(* dummpy check if month is in months *)
	fun contains(months : int list, month : int) = 
	    if null months
	    then false
	    else
		let
		    val first = hd months
		in
		    if first = month
		    then true
		    else contains((tl months), month)
		end

	(* remove the duplicate element *)
	fun deduplicate(months : int list) = 
	    if  null months
	    then months
	    else
		let
		    val rest = deduplicate((tl months))
		    val first = hd months				  
		in
		    if contains(rest, first)
		    then rest
		    else first :: rest
		end
    in
	let
	    val months = deduplicate(months)
	in
	    if null months
	    then 0
	    else number_in_month(dates, (hd months)) + number_in_months_challenge(dates, (tl months))
	end
    end	    

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    let
	(* dummpy check if month is in months *)
	fun contains(months : int list, month : int) = 
	    if null months
	    then false
	    else
		let
		    val first = hd months
		in
		    if first = month
		    then true
		    else contains((tl months), month)
		end
	(* remove the duplicate element *)
	fun deduplicate(months : int list) = 
	    if  null months
	    then months
	    else
		let
		    val rest = deduplicate((tl months))
		    val first = hd months				  
		in
		    if contains(rest, first)
		    then rest
		    else first :: rest
		end
    in
	let
	    val months = deduplicate(months)
	in
	    if null months
	    then []
	    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))
	end
    end

(* A “real date” has a positive year (year 0 did not exist), a month between 1 and 12, 
and a day appropriate for the month. Solutions should properly handle leap years. Leap 
years are years that are either divisible by 400 or divisible by 4 but not divisible by 
100. (Do not worry about days possibly lost in the conversion to the Gregorian calendar 
in the Late 1500s.)*)
fun reasonable_date (date : int * int * int ) = 
    let 
	val year = #1 date
	val month = #2 date
	val day = #3 date
	val leap_year_days_of_month = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val normal_year_days_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	fun is_leap(year : int) = 
	    (year mod 400 = 0) orelse ((year mod 4 = 0) andalso not (year mod 100 = 0))

	fun get_nth(ints : int list, n : int) = 
	    if n = 1
	    then hd ints
	    else get_nth((tl ints), n-1)		       
    in
	if year <=0 orelse month < 1 orelse month > 12 orelse day <= 0
	then false
	else day <= get_nth(if is_leap(year) then leap_year_days_of_month else normal_year_days_of_month, month)
    end
