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

(* 1 *)
fun only_capitals xlist =
    List.filter (fn x =>  Char.isUpper (String.sub (x, 0))) xlist

(* 2 *)
fun longest_string1 xlist =
    List.foldl (fn (x,y) => if not (String.size x > String.size y) then y  else x) "" xlist

(* 3 *)
fun longest_string2 xlist =
    List.foldl (fn (x,y) => if String.size x < String.size y then y else x) "" xlist

(* 4 *)
fun longest_string_helper f =
    fn xlist => List.foldl (fn (x, y) => if not (f(String.size x, String.size y)) then y else x) "" xlist

val longest_string3 = longest_string_helper (fn (x, y) =>  x>y)

val longest_string4 = longest_string_helper (fn (x, y) => x>=y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string  = String.implode o List.rev o String.explode
    
(* 7 *)
fun first_answer f =
   fn xlist => case List.filter (fn x => isSome (f(x))) xlist of
		    [] => raise NoAnswer
		  | head :: rest => valOf (f head)

(* 8 *)
fun all_answers f =
    fn xlist => let fun helper(xlist, result) =
			case xlist of
			    [] => SOME result
			  | head :: xlist' => case (f head) of
						  NONE => NONE
						| SOME blist => helper(xlist', result @ blist)
		in
		    helper(xlist, [])
		end
		   
(* 9 *)
val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size x)
    
fun count_some_var (xstring, xpattern) =
    g (fn x=> 0) (fn x => if x = xstring then 1 else 0) xpattern

(* 10 *)
fun check_pat xpat =
    let fun pattolist xpat =
	    case xpat of
		Variable xstring => [xstring]
	      | TupleP xlist  => List.foldl (fn (x, y) => (pattolist x) @ y) [] xlist
	      | ConstructorP ( _, apat) => pattolist apat
	      | _ => []
    in
	let fun ifrepeats xlist =
		case xlist of
		    [] => true
		  | headlist :: xlist' => not (List.exists (fn x => x = headlist) xlist') andalso ifrepeats xlist'
	in
	    ifrepeats (pattolist xpat)
	end
    end

(* 11 *)
fun match (v, p) =
    case p of
	Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => (case v of
		     Unit => SOME []
		   | _ => NONE)
      | ConstP x => (case v of
			Const y => if x=y then SOME [] else NONE
		      | _ => NONE)
      | TupleP ps => (case v of
			  Tuple vs => if List.length ps = List.length vs then all_answers (fn (x, y) => if isSome (match(x, y)) then match(x, y) else NONE) (ListPair.zip(vs, ps)) else NONE
			| _ => NONE)
      | ConstructorP (s1, p) => (case v of
				     Constructor (s2, v) => if s1 = s2 andalso isSome (match(v, p)) then match(v, p) else NONE
				   | _ => NONE)
					      
(* 12 *)				 
fun first_match v ps =
   SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
    
	
