(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
	     
(* 1 *)
fun all_except_option (xstring, xlist) =
    let fun helper(xstring, xlist, templist) = 
	    case xlist of
		[] => NONE
	      | headstr :: xlist' => if same_string(headstr, xstring)
			           then SOME (templist @ xlist')
			           else helper(xstring, xlist', (templist @ [headstr]) )
    in helper(xstring, xlist, [])
    end

(* 2 *)
fun get_substitutions1 (xlist, s) =
    case xlist of
	[] => []
      | headlist :: xlist' => case all_except_option(s, headlist) of
			 NONE => get_substitutions1(xlist', s)
			| SOME templist => templist @ get_substitutions1(xlist', s)

(* 3 *)									
fun get_substitutions2 (xlist, s) =
    let fun helper(xlist, s, resultlist) =
	case xlist of
	    [] => resultlist
	  | headlist :: xlist' => case all_except_option(s, headlist) of
		                 NONE => helper(xlist', s, resultlist)
		               | SOME templist => helper(xlist', s, resultlist @ templist)
    in helper(xlist, s, [])
    end

(* 4 *)
fun similar_names (xlist, {first=x, middle=y, last=z}) =
    let fun helper(templist, resultlist) =
	case templist of
	    [] => {first=x, middle=y, last=z} :: resultlist
	  | headstr :: templist' =>
	    helper(templist', resultlist @ [{first=headstr, last=z, middle=y}])
    in helper(get_substitutions2(xlist, x), [])
    end
		      			      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 5 *)
fun card_color (suit, rank) =
    case suit of
	Spades  => Black
     | Clubs => Black
     | Diamonds =>Red
     | Hearts => Red

(* 6 *)				      
fun card_value (suit, rank) =
    case rank of
	Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11
      | Num cao => cao

(* 7 *)	
fun remove_card (cs, c, e) =
    let fun helper(cs, c, templist) =
	    case cs of
		[] => raise e
	      | headcard :: cs' => if headcard = c then templist @ cs'
				   else helper(cs', c, templist @ [headcard])
    in helper(cs, c, [])
    end

(* 8 *)
fun all_same_color cs =
    case cs of
	[] => true
      | card1 :: [] => true
      | card1 :: card2 :: cs'' => if card_color(card1) = card_color(card2)
			     then all_same_color(card2 :: cs'')
		                  else false
					   

(* 9 *)
fun sum_cards cs =
    let fun helper(cs, result) =
	    case cs of
	         [] => result
	       | headcard :: cs' => helper(cs', result + card_value(headcard))
    in helper(cs, 0)
    end

(* 10 *)	
fun score (cs, goal) =
    let val tempnum = sum_cards cs - goal
    in
	let val tempbool = all_same_color cs
	in
	    if tempnum > 0 andalso tempbool then 3 * tempnum div 2
	    else if tempnum > 0 andalso not tempbool then 3 * tempnum
	    else if tempnum < 0 andalso tempbool then (0 - tempnum) div 2
	    else 0 - tempnum
	end
    end

(* 11 *)
fun officiate (cardlist, movelist, goal) =
    let fun helper(handlist, cardlist, movelist, goal) =
	    case movelist of
		[] => score(handlist, goal)
	      | headmove :: movelist' => case headmove of
					     Discard somecard => helper(remove_card(handlist, somecard, IllegalMove), cardlist, movelist', goal)
					  |  Draw => case cardlist of
							 [] => score(handlist, goal)
						       | headcard :: cardlist' => if (sum_cards handlist) + card_value headcard  > goal
										  then score(headcard :: handlist, goal)
										  else helper(headcard :: handlist, cardlist', movelist', goal)
    in
	helper([], cardlist, movelist, goal)
    end
	
											
		
	   
