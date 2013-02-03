(* HW3 Tests *)
use "hw3.sml";
(* Problem 1 *)
val names = ["Andy","Bernard","Cathy","Doug","Elaine","Fred","Greg","Harry","Isis","Jack","Karen","Laura","Mike","Nancy","Ophelia","Petunia","Quincy","Ray","Supercalifragilisticexpialidocious","Tom","Uma","Velma","Wilma","Xan","Yan","Zed"]

(* names2 is same as names, but longest word is lower case *)
val names2 = ["Andy","Bernard","Cathy","Doug","Elaine","Fred","Greg","Harry","Isis","Jack","Karen","Laura","Mike","Nancy","Ophelia","Petunia","Quincy","Ray","supercalifragilisticexpialidocious","Tom","Uma","Velma","Wilma","Xan","Yan","Zed"]

val names3 = ["andy","bernard","cathy","doug","elaine"]

val empty = []
val list1b = ["Andy","bernard","Cindy"]
val list1c = ["andy","bernard","Cindy"]
val list1d = ["andy"]

val test1a = only_capitals(empty)=[]
val test1b = only_capitals(list1b)=["Andy","Cindy"]
val test1c = only_capitals(list1c)=["Cindy"]
val test1d = only_capitals(list1d)=[]

(* Problem 2 *)
val list2a = ["Andy","Billy","Blair"]

val test2a = longest_string1(empty) = ""
val test2b = longest_string1(list1b) = "bernard"
val test2c = longest_string1(names) = "Supercalifragilisticexpialidocious"
val test2d = longest_string1(list2a) = "Billy"

(* Problem 3 *)
val test3a = longest_string2(empty) = ""
val test3b = longest_string2(list1b) = "bernard"
val test3c = longest_string2(names) = "Supercalifragilisticexpialidocious"
val test3d = longest_string2(list2a) = "Blair"

(* Problem 4 *)
val test4a = longest_string3(empty) = ""
val test4b = longest_string3(list1b) = "bernard"
val test4c = longest_string3(names) = "Supercalifragilisticexpialidocious"
val test4d = longest_string3(list2a) = "Billy"

val test4e = longest_string4(empty) = "";
val test4f = longest_string4(list1b) = "bernard";
val test4g = longest_string4(names) = "Supercalifragilisticexpialidocious";
val test4h = longest_string4(list2a) = "Blair";

print "Problem 5\n";
val test5a = longest_capitalized(names) = "Supercalifragilisticexpialidocious";
val test5b = longest_capitalized(names2) = "Bernard";
val test5c = longest_capitalized(names3) = "";
val test5d = longest_capitalized([]) = "";

print "Problem 6\n";
rev_string "asd" = "dsa";
rev_string "" = "";

print "Problem 7\n";
first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4;
first_answer (fn x => if x < 0 then SOME x else NONE) [1,2,3,4,5]
handle 
NoAnswer => ~1;

print "Problem 8\n";
all_answers (fn x => x) [] = SOME [];
all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE ) [2,4,6] = SOME [2,4,6];
all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE ) [2,4,6,3] = NONE;

print "Problem 9\n";
count_wildcards Wildcard = 1;
count_wildcards UnitP = 0;
count_wildcards (ConstP 12)= 0;
count_wildcards (TupleP [ (TupleP [Wildcard, UnitP]), (TupleP [Wildcard, UnitP]) ])= 2;
count_wild_and_variable_lengths (TupleP [Wildcard, Variable("asd")]) = 4;
count_some_var ("tar", (TupleP [ConstructorP("a", UnitP), Variable("tar"), Variable("taa")])) = 1;

print "Problem 10\n";
check_pat (TupleP [Variable("x"),Variable("x")]) = false;
check_pat (TupleP [Variable("x"),ConstructorP("wild", Wildcard)]) = true;
check_pat (TupleP [TupleP [Variable("x"),ConstructorP("wild", Wildcard)],Variable("x")]) = false;
check_pat (TupleP [TupleP [Variable("x"),ConstructorP("wild", Wildcard)],Wildcard]) = true;
check_pat (TupleP [TupleP [TupleP [Variable("x"),ConstructorP("wild", Wildcard)],Wildcard],Variable("x")]) = false;
check_pat (ConstructorP("egg", (ConstructorP("egg", (ConstP 4))))) = true;
check_pat (TupleP [ConstP 17,Wildcard,ConstP 4,ConstructorP("egg", (ConstP 4)),ConstructorP("egg", ConstructorP("egg", ConstP 4)) ]) = true;
check_pat (TupleP [Wildcard,Wildcard]) = true;
check_pat (TupleP [ConstP 17,ConstP 4]) = true;
check_pat (TupleP [ConstructorP("egg", (ConstP 4)),ConstructorP("egg", (ConstP 4))]) = true;

print "Problem 11\n";
match ( Const 1, ConstP 2 ) = NONE;
match ( Const 1, ConstP 1 ) = SOME [];
match ( Constructor("Num", Const 1), Variable("b")) = SOME [ ("b", Constructor("Num", Const 1)) ];
match ( Const 1, Variable("b")) = SOME [ ("b", Const 1) ];
match ( Constructor("Num", Const 1), UnitP) = NONE;
match ( Tuple [Const 1, Unit], TupleP [Variable("a"), Variable("b")]) = SOME [ ("a", Const 1), ("b", Unit)];

print "challenge problem\n";
typecheck_patterns ([], [ TupleP[Variable("x"),Variable("y")], TupleP[Wildcard,Wildcard] ]) = SOME (TupleT [Anything, Anything]);
typecheck_patterns ([], [ TupleP[Wildcard,Wildcard], TupleP[Wildcard,TupleP[Wildcard,Wildcard]] ]) = SOME (TupleT [Anything,TupleT [Anything,Anything]]);

val datatyps = [("King", "Rank", UnitT), ("Queen", "Rank", UnitT), 
                ("Jack", "Rank", UnitT), ("Num", "Rank", IntT)];
val patterns = [ConstructorP("King", UnitP), ConstructorP("Num", TupleP[Wildcard])];
typecheck_patterns (datatyps, patterns) = NONE;
