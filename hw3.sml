(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1-6 *)
fun only_capitals strs = 
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) strs
		
fun longest_string1 strs = 
    let
	fun update(next, current) = 
	    if String.size next > String.size current
	    then next
	    else current
    in
	foldl update "" strs
    end

fun longest_string2 strs = 
    let
	fun update(next, current) = 
	    if String.size next >= String.size current
	    then next
	    else current
    in
	foldl update "" strs
    end

fun longest_string_helper f strs = 
    let
	fun update(next, current) = 
	    if f(String.size(next), String.size(current))
	    then next
	    else current
    in
	foldl update "" strs
    end

val longest_string3 = longest_string_helper (fn (next, current) => next >  current)
val longest_string4 = longest_string_helper (fn (next, current) => next >= current)
val longest_capitalized = longest_string1 o only_capitals
val rev_string = String.implode o rev o String.explode

(* 7-8 *)
fun first_answer pred questions = 
    case questions of
	[] => raise NoAnswer
      | x::xs => case pred x of
		     NONE => first_answer pred xs
		   | SOME v => v

fun all_answers pred questions = 
    let
	fun answer_helper result ques = 
	    case ques of
		[] => SOME result
	      | x::xs => case pred x of
			     NONE => NONE
			   | SOME fst => answer_helper (result @ fst) xs
    in
	answer_helper [] questions
    end

(* 9-12 *)
val count_wildcards = g (fn x => 1) (fn x => 0)
val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size x)
fun count_some_var (str, p) = g (fn x => 0) (fn s => if s = str then 1 else 0) p

fun check_pat p =
    let
	fun var_names pat = 
	    case pat of
		Wildcard          => []
	      | Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p,i) => (var_names p) @ i) [] ps
	      | ConstructorP(_,p) => []
	      | _                 => []
	fun check_repeat name_list = 
	    case name_list of
		[] => false
	      | x::xs => (List.exists (fn name => name = x) xs) orelse check_repeat xs
    in
	not (check_repeat(var_names(p)))
    end

fun match (value, pattern) =
    case (pattern, value) of
	(Wildcard, _) => SOME []
      | (Variable(var), v) => SOME [(var, v)]
      | (UnitP, Unit) => SOME []
      | (ConstP(a), Const b) => if a <> b then NONE else SOME []
      | (ConstructorP(s1,p), Constructor(s2,v)) => if s1 = s2 then match(v, p) else NONE
      | (TupleP(ps), Tuple(vs)) => 
	if List.length ps <> List.length vs
	then NONE
	else (case all_answers match (ListPair.zip(vs, ps)) of
		 NONE => NONE
	       | SOME lst => SOME lst)
      | _ => NONE

fun first_match v patterns = 
    SOME (first_answer (fn p => match (v, p)) patterns)
    handle NoAnswer => NONE

(* Challenge *)
fun all_results pred questions = 
    let
	fun answer_helper result ques = 
	    case ques of
		[] => SOME (List.rev result)
	      | x::xs => case pred x of
			     NONE => NONE
			   | SOME fst => answer_helper (fst::result) xs
    in
	answer_helper [] questions
    end

(* get the type of a pattern *)
fun type_of_pattern table pat = 
    let
	val r = type_of_pattern table
	(* look up a constructor's type *)
	fun type_of_constructor lookup_table name para = 
	    case lookup_table of
		[] => NONE
	      | (cons_name, data_type, t)::xs => (
		  case r para of
		      NONE => NONE
		    | SOME tmp_t => 
		      if cons_name = name andalso t = tmp_t
		      then SOME (Datatype(data_type))
		      else type_of_constructor xs name para
	      )
    in
	case pat of
	    UnitP => SOME UnitT
	  | ConstP _ => SOME IntT
	  | Variable _ => SOME Anything
	  | TupleP(ps) => (case all_results r ps of
			      NONE => NONE
			    | SOME lst => SOME (TupleT(lst)))
	  | ConstructorP(s, p) => type_of_constructor table s p 
	  | Wildcard => SOME Anything
    end

(* compare two type, return more general one, will return NONE if the two are not compatible *)
fun more_general_one (t1,t2) = 
    case (t1, t2) of
	(UnitT, UnitT) => SOME t1
      | (IntT, IntT) => SOME t1
      | (Datatype d1, Datatype d2) => if d1 = d2 then SOME t1 else NONE
      | (TupleT _, Anything) => SOME t1
      | (Anything, TupleT _) => SOME t2
      | (Anything, _) => SOME t2
      | (_, Anything) => SOME t1
      | (TupleT(l1), TupleT(l2)) => 
	(
	  if List.length l1 <> List.length l2
	  then NONE
	  else
	      case all_results more_general_one (ListPair.zip(l1, l2)) of
		  NONE => NONE
		| SOME lst => SOME (TupleT(lst)))
      | _ => NONE

(* find the most general *)
fun filter_most_general result lst =
    case lst of
	[] => SOME result
      | [x] => (case more_general_one(result,x) of
		    NONE => NONE
		  | SOME r => SOME r
	       )
      | a::b::xs => case more_general_one(a,b) of
			NONE => NONE
		      | SOME t => filter_most_general t xs

fun typecheck_patterns (table, pats) = 
    let
	val types = all_results (type_of_pattern table) pats
	(* check illegal variable case *)
	val check_pat_result = all_results (fn p => if check_pat p then SOME p else NONE) pats
    in
	case check_pat_result of
	    NONE => NONE
	  | SOME _ => 
	    (
	      case types of
		  NONE => NONE
		| SOME lst => filter_most_general Anything lst
	    )
    end
