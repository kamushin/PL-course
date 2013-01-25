(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, string_list) = 
    case string_list of
	[] => NONE
      | x::xs => let val rest = all_except_option(s, xs)
		 in
		     case (same_string(x, s), rest) of
			 (true, NONE) => SOME xs
		       | (true, SOME lst) => SOME lst
		       | (false, NONE) => NONE
		       | (false, SOME lst) => SOME (x::lst)
		 end


fun get_substitutions1 (substitutions, s) = 
    case substitutions of 
	[] => []
      | x::xs => 
	case all_except_option(s, x) of 
	    NONE => get_substitutions1(xs, s)
	  | SOME lst => lst @ get_substitutions1(xs, s)

fun get_substitutions2 (substitutions, s) = 
    let 
	fun get_sub(subs, s, accu) = 
	    case subs of 
		[] => accu
	      | x::xs => case all_except_option(s, x) of
			    NONE => get_sub(xs, s, accu)
			  | SOME lst => get_sub(xs, s, accu @ lst)
    in
	get_sub(substitutions, s, [])
    end

fun similar_names (substitutions, {first=first, middle=middle, last=last}) = 
    let
	fun get_sim_name(sim_names) = 
	    case sim_names of
		[] => []
	      | x::xs => {first=x, middle=middle, last=last}:: get_sim_name(xs)
    in
	{first=first, middle=middle, last=last} :: get_sim_name(get_substitutions2(substitutions, first))
    end

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (c) = 
    case c of
	(Spades,_) => Black
      | (Clubs,_) => Black
      | _ => Red

fun card_value (c) = 
    case c of 
	(_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
     | x::xs => if x = c then xs else x::remove_card(xs, c, e) 

						    
fun all_same_color (cs) = 
    case cs of
	[] => true
      | x::[] => true
      | c1::c2::xs => card_color(c1) = card_color(c2) andalso all_same_color(c2::xs)

fun sum_cards (cds) = 
    let
	fun sum(cards, accu) = 
	    case cards of
		[] => accu
	      | x::xs => sum(xs, card_value(x) + accu)
    in
	sum(cds, 0)
    end

fun score (cards, goal) = 
    let
	val sum = sum_cards(cards)
	val prilim_score = if sum > goal then 3 * (sum - goal) else goal -sum
    in
	if all_same_color(cards)
	then
	    prilim_score div 2
	else
	    prilim_score
    end

fun officiate (cards, moves, goal) = 
    let
	fun play(card_list, held_cards, move_list) = 
	    case move_list of
		[] => score(held_cards, goal)
	      | (Discard c)::xs => play(card_list, remove_card(held_cards, c, IllegalMove), xs)
	      | (Draw)::xs => case card_list of 
				[] => score(held_cards, goal)
			      | draw::rest => if sum_cards(draw::held_cards) > goal 
					      then score(draw::held_cards, goal) 
					      else play(rest, draw::held_cards, xs)
    in
	play(cards, [], moves)
    end

fun score_challenge (cards, goal) = 
    let
	val sum = sum_cards(cards)
	val prilim_score = if sum > goal then 3 * (sum - goal) else goal -sum
	val prilim_score2 = if (sum-10) > goal then 3 * (sum - 10 - goal) else goal - sum + 10									      
    in
	if all_same_color(cards)
	then
	    if prilim_score < prilim_score2 then prilim_score div 2 else prilim_score2 div 2
	else
	    if prilim_score < prilim_score2 then prilim_score else prilim_score2
    end

fun officiate_challenge (cards, moves, goal) = 
    let
	fun play(card_list, held_cards, move_list) = 
	    case move_list of
		[] => score(held_cards, goal)
	      | (Discard c)::xs => play(card_list, remove_card(held_cards, c, IllegalMove), xs)
	      | (Draw)::xs => case card_list of 
				[] => score(held_cards, goal)
			      | draw::rest => if sum_cards(draw::held_cards) > goal 
					      then score(draw::held_cards, goal) 
					      else play(rest, draw::held_cards, xs)
    in
	play(cards, [], moves)
    end

