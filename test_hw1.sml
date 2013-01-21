use "hw1.sml";

(* test is_older *)
is_older((1,2,3), (1,2,3)) = false;
is_older((1,2,2), (1,2,3)) = true;
is_older((2,2,3), (1,2,3)) = false;

(* test number_in_month *)
number_in_month( [(1,2,3), (1,2,3), (2,2,3)], 2 ) = 3;
number_in_month( [(1,2,3), (1,2,3), (2,1,3)], 2 ) = 2;
number_in_month( [(1,3,3), (1,4,3), (2,1,3)], 2 ) = 0;
number_in_month( [], 2 ) = 0;

(* test number_in_months *)
number_in_months( [], [] ) = 0;
number_in_months( [(1,2,3)], [] ) = 0;
number_in_months( [(1,2,3)], [2] ) = 1;
number_in_months( [(1,2,3)], [1,2] ) = 1;
number_in_months( [(1,2,3), (1,1,3)], [1,2] ) = 2;
number_in_months( [(1,2,3), (1,1,3), (1,3,3)], [1,2] ) = 2;

(* test dates_in_month *)
dates_in_month( [], 2 ) = [];
dates_in_month( [(1,2,3)], 2 ) = [(1,2,3)];
dates_in_month( [(1,2,3)], 2 ) = [(1,2,3)];
dates_in_month( [(1,2,3), (1,1,1)], 2 ) = [(1,2,3)];

(* test dates_in_months *)
dates_in_months( [], [] ) = [];
dates_in_months( [], [1] ) = [];
dates_in_months( [], [1,2] ) = [];
dates_in_months( [(1,2,3)], [1] ) = [];
dates_in_months( [(1,2,3)], [1,2] ) = [(1,2,3)];
dates_in_months( [(1,2,3), (2,2,2)], [1,2,4] ) = [(1,2,3), (2,2,2)];
dates_in_months( [(1,2,3), (2,2,2)], [1,2,3] ) = [(1,2,3), (2,2,2)];
dates_in_months( [(1,2,3), (2,2,2), (3,3,3)], [1,2,3] ) = [(1,2,3), (2,2,2), (3,3,3)];

(* test get_nth *)
get_nth( ["1","2","3","4","5"], 1) = "1";
get_nth( ["1","2","3","4","5"], 2) = "2";

(* test date_to_string *)
date_to_string( (1,1,1) ) = "January 1, 1";
date_to_string( (2012,1,1) ) = "January 1, 2012";

(* test number_before_reaching_sum *)
number_before_reaching_sum(1, [1,2,3]) = 0;
number_before_reaching_sum(2, [1,2,3]) = 1;
number_before_reaching_sum(3, [1,2,3]) = 1;
number_before_reaching_sum(4, [1,2,3]) = 2;

(* test what_month *)
what_month(1) = 1;
what_month(32) = 2;
what_month(59) = 2;
what_month(60) = 3;

(* test month_range *)
month_range(1,2) = [1,1];
month_range(30,31) = [1,1];
month_range(30,32) = [1,1,2];

(* test oldest *)
oldest( [(1,2,3)] ) = SOME (1,2,3);
oldest( [] ) = NONE;

(* test number_in_months_challenge *)
number_in_months_challenge( [], [] ) = 0;
number_in_months_challenge( [(1,2,3)], [] ) = 0;
number_in_months_challenge( [(1,2,3)], [2] ) = 1;
number_in_months_challenge( [(1,2,3)], [1,2] ) = 1;
number_in_months_challenge( [(1,2,3), (1,1,3)], [1,2] ) = 2;
number_in_months_challenge( [(1,2,3), (1,1,3), (1,3,3)], [1,2,2] ) = 2;

(* tes dates_in_months_challenge *)
dates_in_months_challenge( [], [] ) = [];
dates_in_months_challenge( [], [1] ) = [];
dates_in_months_challenge( [], [1,2] ) = [];
dates_in_months_challenge( [(1,2,3)], [1] ) = [];
dates_in_months_challenge( [(1,2,3)], [1,2] ) = [(1,2,3)];
dates_in_months_challenge( [(1,2,3), (2,2,2)], [1,2,4] ) = [(1,2,3), (2,2,2)];
dates_in_months_challenge( [(1,2,3), (2,2,2)], [1,2,2,3] ) = [(1,2,3), (2,2,2)];
dates_in_months_challenge( [(1,2,3), (2,2,2), (3,3,3)], [1,2,3,2,3,1,3] ) = [(1,2,3), (2,2,2), (3,3,3)];

(* test reasonable_date *)
reasonable_date(2012, 1, 31) = true;
reasonable_date(2000, 2, 29) = true;
reasonable_date(1000, 2, 29) = false;

