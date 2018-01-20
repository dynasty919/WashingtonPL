fun is_older(x1:(int * int* int), x2:(int * int * int)) =
    if #1 x1 < #1 x2 then true
    else if #1 x1 > #1 x2 then false
    else if #2 x1 < #2 x2 then true
    else if #2 x1 > #2 x2 then false
    else if #3 x1 < #3 x2 then true
    else false

fun number_in_month(x1: (int * int * int) list, x2:int) =
    if null x1 then 0
    else let fun first(x1: (int * int * int) list, x2:int) = if #2 (hd x1) = x2 then 1 else 0
	 in first(x1,x2) + number_in_month(tl x1, x2)
	 end

fun number_in_months(x1:(int * int * int) list, x2: int list) =
    if null x2 then 0 else number_in_month(x1, hd x2) +  number_in_months(x1, tl x2)
						     
fun dates_in_month(x1: (int * int * int) list, x2:int) =
    if null x1 then []
    else let fun first(x1: (int * int * int) list, x2:int) =
		 if #2 (hd x1) = x2 then SOME (hd x1) else NONE
	 in if isSome (first(x1,x2)) then valOf (first(x1, x2)) :: dates_in_month(tl x1, x2)
	    else dates_in_month(tl x1, x2)
	 end
	     
fun dates_in_months(x1:(int * int * int) list, x2: int list) =
    if null x2 then [] else dates_in_month(x1,hd x2) @ dates_in_months(x1, tl x2)

fun get_nth(x : string list, n : int) =
    if n = 1 then hd x else get_nth(tl x, n-1)

fun date_to_string(x: (int * int * int)) =
    let val monthlist = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(monthlist, #2 x) ^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x)
    end
	
fun number_before_reaching_sum(sum: int, x: int list) =
    if (hd x) < sum then 1 + number_before_reaching_sum(sum - (hd x), tl x) else 0
							    
fun what_month(x : int) =
    let val temp = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 1 + number_before_reaching_sum(x, temp)
    end

fun month_range(x : int, y : int) =
    if x > y then [] else what_month(x) :: month_range( x + 1, y)

fun oldest(x : (int * int * int) list) =
    if null x then NONE
    else let val temp = oldest(tl x)
	 in
	     if isSome temp andalso not ( is_older(hd x, (valOf temp)) ) 
	     then temp else  SOME (hd x) 
	 end
	     
