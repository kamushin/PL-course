(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
use "hw2.sml";

(*
fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,41)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end
*)

all_except_option("a", ["a","b"]) = (SOME ["b"]);
all_except_option("a", []) = NONE;
all_except_option("c", ["a","b"]) = NONE;

get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                   "Fred") = ["Fredrick","Freddie","F"];
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                           "Jeff") = ["Jeffrey","Geoff","Jeffrey"];

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                   "Fred") = ["Fredrick","Freddie","F"];
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                           "Jeff") = ["Jeffrey","Geoff","Jeffrey"];


similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                      {first="Fred", middle="W", last="Smith"}) = 
[{first="Fred", last="Smith", middle="W"},
 {first="Fredrick", last="Smith", middle="W"},
 {first="Freddie", last="Smith", middle="W"},
 {first="F", last="Smith", middle="W"}];


(* problem 2*)
card_color( (Clubs, Jack) )    = Black;
card_color( (Diamonds, Jack) ) = Red;

card_value( (Clubs, (Num 2)) ) = 2;

val cards = [ (Clubs, (Num 3)), (Spades, Ace), (Spades, Jack), (Hearts, (Num 6)) ];

remove_card(cards, (Clubs, (Num 3)), IllegalMove) = [(Spades, Ace), (Spades, Jack), (Hearts, (Num 6)) ];

