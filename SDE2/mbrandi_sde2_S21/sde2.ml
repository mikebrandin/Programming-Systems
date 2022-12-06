(** Mike Brandin 12/1/2021*)
(** TODO: first_duplicate*)
let rec in_list = function 
    (list, i) ->
      if (list = []) then false 
      else if (i = List.hd list) then true
      else in_list(List.tl list, i);;

let rec first_duplicate = function
    (list) -> 
      if (list = []) then -10000
      else if in_list(List.tl list, List.hd list) then List.hd list
      else first_duplicate(List.tl list);;

(** TODO: first_nonrepeating*)
let rec not_in_list = function 
    ([], list2) -> -10000
  | (h::t, list2) -> 
      if not (List.mem h t || List.mem h list2) then h 
      else not_in_list(t, h::list2);;

let rec first_nonrepeating = function
    (list) -> 
      not_in_list(list, []);;
    
(** TODO: sumOfTwo*)
let rec sumOfTwo_helper = function 
    (list1, [], v) -> false
  | (list1, list2, v) ->
      if (List.hd list1 + List.hd list2 = v) then true
      else sumOfTwo_helper(list1, List.tl list2, v);;

let rec sumOfTwo = function
    ([], [], v) -> false
  | ([], list2, v) -> false
  | (list1, list2, v) ->
      if sumOfTwo_helper(list1, list2, v) then true
      else sumOfTwo(List.tl list1, list2, v);;
          
(** TODO: cyk_sublists*)
let rec cyk_helper = function 
    (max, i, list) -> 
      if ((i - max) > 0) 
      then cyk_helper(max+1, i, (abs(i-max), max)::list)
      else list;;

let rec cyk_sublists = function
    (n) -> cyk_helper(1, n, []);;