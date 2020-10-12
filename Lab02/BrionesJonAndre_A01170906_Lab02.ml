open Stdio

let testlst1 = [4;2;6;7;6;8;1]
let testlst2 = [3;2;7;6;8;1]
let testlst3 = [3;2;2;2;7;3;3;6;2;8;8]

(* Tests whether two lists are equal *)
let rec equal (l1: 'a list) (l2: 'a list) =
  match (l1, l2) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (h1::t1, h2::t2) -> h1 = h2 && equal t1 t2

(* Test function 
 * test_name is the name of the function being tested
 * t is the test being performed
 * e is the expected value
 * Prints whether the test passes or fails
*)
let test test_name t e =
  match equal t e with 
  | true  ->  printf "%s...PASSED\n" test_name
  | false ->  printf "%s...FAILED\n." test_name

(* Question 1a 
 * Drops terms that matches the f's conditions, returning once a
 * a condition is not met
 * f is a function that returns true or false
 * lst is a list to checks*) 
let rec drop_while f lst =
  match lst with 
  | [] -> []
  | h::t -> if f h 
    then drop_while f t
    else lst

let () = test "drop_while test 1" (drop_while (fun x -> x mod 2 = 0) testlst1) [7;6;8;1] 
let () = test "drop_while test 2" (drop_while (fun x -> x mod 2 = 1) testlst2) [2;7;6;8;1] 
let () = test "drop_while test 3" (drop_while (fun x -> x mod 2 = 0) []) [] 

(* Question 1b 
 * lst is the input list
 * acc is the accumulator to ensure tail recursion
*)
let filter f lst =
  let rec filter' lst acc =
    match lst with
    | [] -> acc
    | h::t -> if f h 
      then filter' t (h::acc)
      else filter' t acc
  in List.rev (filter' lst [])

let () = test "filter test 1" (filter (fun x -> x mod 2 = 0) testlst2) [2;6;8]
let () = test "filter test 2" (filter (fun x -> x mod 2 = 1) testlst2) [3;7;1]
let () = test "filter test 3" (filter (fun x -> x mod 2 = 0) []) []

(* Question 1c 
 * lst1' is the first list being zipped in
 * lst2' is the second list being zipped in
 * acc is the accumulator to ensure tail recursion
*)
let zip lst1 lst2 =
  let rec zip' lst1' lst2' acc =
    match lst1', lst2' with
    | [], [] -> []
    | _, [] | [], _ -> acc
    | h1::t1, h2::t2 -> zip' t1 t2 ((h1, h2)::acc)
  in List.rev (zip' lst1 lst2 [])

let () = test "zip test 1" (zip [1;2;3] ['a';'b';'c';'d']) [(1,'a');(2,'b');(3,'c')]
let () = test "zip test 2" (zip [] []) []
let () = test "zip test 3" (zip ['a';'b';'c';'d'] [1;2;3]) [('a',1);('b',2);('c',3)]

(* Question 1d 
 * a is the lower bound
 * b is the upper bound
 * lower is the lower bound 
 * acc is the accumulator to ensure tail recursion
*)
let range a b =
  if a >= b
  then []
  else let rec range' lower acc =
         if lower > b
         then acc
         else range' (lower+1) (lower::acc)
    in List.rev (range' a [])

let () = test "range test 1" (range (-1) 3) [-1;0;1;2;3]
let () = test "range test 2" (range 3 (-1)) []
let () = test "range test 3" (range (-5) 0) [-5;-4;-3;-2;-1;0]

(* Question 2a
 * lst1 is the first sorted list (smallest to largest) to merge 
 * lst2 is the second sorted list (smallest to largest) to merge 
 * lst1' is the list in the inner function to merge
 * lst2' is the list in the inner function to merge
 * acc is the accumulator to ensure tail recursion
*)
let merge_ints (lst1:int list) (lst2: int list) =
  let rec merge_ints' lst1' lst2' acc =
    match lst1', lst2' with
    | [],[] -> acc
    | h::t, [] | [], h::t-> merge_ints' t [] (h::acc)
    | h1::t1, h2::t2 -> if h1 < h2
      then merge_ints' t1 lst2 (h1::acc)
      else merge_ints' lst1 t2 (h2::acc)
  in List.rev (merge_ints' lst1 lst2 [])

let () = test "merge_ints test 1" (merge_ints [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let () = test "merge_ints test 1" (merge_ints [] []) []
let () = test "merge_ints test 2" (merge_ints [1;2;3;4;5;6] []) [1;2;3;4;5;6]

(* Question 2b
 * f is the function being used to compare the values
 * lst1 is the first sorted list (smallest to largest) to merge 
 * lst2 is the second sorted list (smallest to largest) to merge  
 * lst1' is the list in the inner function to merge
 * lst2' is the list in the inner function to merge
 * acc is the accumulator to ensure tail recursion
*)
let merge f lst1 lst2 =
  let rec merge' lst1' lst2' acc =
    match lst1', lst2' with
    | [], [] -> acc
    | h::t, [] | [], h::t -> merge' t [] (h::acc)
    | h1::t1, h2::t2 -> if (f h1 h2)
      then merge' t1 lst2' (h1::acc)
      else merge' lst1' t2 (h2::acc)
  in List.rev (merge' lst1 lst2 [])

let () = test "merge test 1" (merge (fun a b -> a < b) [4;5;6] [1;2;3]) [1;2;3;4;5;6]
let () = test "merge test 2" (merge (fun a b -> a < b) [] []) []
let () = test "merge test 3" (merge (fun a b -> 2*a < b) [1;2;4] [3;5;9]) [1;3;2;5;4;9] 

(* Question 3 
 * lst is the list that's having its consecutive elements grouped together
 * lst' is the list passed in to check with the helper function
 * curr is the current consecutive value list
 * acc is the accumulated list of all curr lists
*)                  
let group (lst: int list) = 
  let rec group' lst' curr acc =
    match lst' with
    | [] -> []
    | [x] -> (x::curr)::acc
    | x::(y::_ as t) -> if x = y 
      then group' t (x::curr) acc
      else group' t [] ((x::curr)::acc) 
  in List.rev (group' lst [] [])

let () = test "group 1 test" (group testlst3) [[3];[2;2;2];[7];[3;3];[6];[2];[8;8]]
let () = test "group 2 test" (group []) []
let () = test "group 3 test" (group [1;1;1]) [[1;1;1]]
