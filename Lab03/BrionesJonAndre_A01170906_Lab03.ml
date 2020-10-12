open Base
open Stdio

let testlist1 = ["hello";"hello";"goodbye";"hello";"howdy"]
let testlist2 = ["howdy";"bye";"hi";"bye";"world";"hi";"hello";"world";
                 "hi";"hi";"bye";"hi";"hi"]

let explist1 = [["hello";"hello"];["goodbye"];["hello"];["howdy"]]
let explist2 = [["howdy"];["bye"];["hi"];["bye"];["world"];["hi"];["hello"];
                ["world"];["hi"; "hi"];["bye"];["hi";"hi"]]
let explist3 = [("bye", 3);("hello", 1);("hi", 6);("howdy", 1);("world", 2)]
let explist4 = [("goodbye",1);("hello",3);("howdy",1)]

(* Matches tuples *)
let match_tup a b =
  match (a, b) with
  | (k1,v1), (k2,v2) -> String.equal k1 k2 && v1 = v2

(* Tests whether two integer lists are equal *)
let rec equal_i (l1:'a list) (l2:'a list) =
  match (l1, l2) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (h1::t1, h2::t2) -> h1 = h2 && equal_i t1 t2

(* Tests whether two string lists are equal *)
let rec equal_s (l1:'a list) (l2:'a list) =
  match (l1, l2) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (h1::t1, h2::t2) -> (String.equal h1 h2) && equal_s t1 t2

(* Tests whether two string * int lists are equal *)
let rec equal_t (l1:(string * int) list) (l2:(string * int) list) =
  match (l1, l2) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (h1::t1, h2::t2) -> match_tup h1 h2 && equal_t t1 t2

(* Tests whether two string list lists are equal *)
let rec equal_ll (l1: 'a list list) (l2: 'a list list) =
  match (l1, l2) with
  | ([], []) -> true
  | ([], _) | (_, []) -> false
  | (h1::t1, h2::t2) -> equal_s h1 h2 && equal_ll t1 t2

(* Test function 
 * test_name is the name of the function being tested
 * t is the test being performed
 * e is the expected value
 * Prints whether the test passes or fails
 *)
let test_s test_name (t: 'a list) (e: 'a list) =
  match equal_s t e with 
  | true  ->  printf "%s...PASSED\n" test_name
  | false ->  printf "%s...FAILED\n." test_name

let test_t test_name (t: (string * int) list) (e: (string * int) list) =
  match equal_t t e with 
  | true  ->  printf "%s...PASSED\n" test_name
  | false ->  printf "%s...FAILED\n." test_name

let test_i test_name (t: int list) (e: int list) =
  match equal_i t e with 
  | true  ->  printf "%s...PASSED\n" test_name
  | false ->  printf "%s...FAILED\n." test_name

let test_l test_name (t: 'a list list) (e: 'a list list) =
  match equal_ll t e with 
  | true  ->  printf "%s...PASSED\n" test_name
  | false ->  printf "%s...FAILED\n." test_name

(* Question 1 
 * Uses filter to remove all multiples of prime numbers
 * n is the positive integer the program will find primes up to (inclusive)
 * lst is the list of numbers from 2 - n
 * acc is the accumulator to ensure tail recursion
 *)
let primes n = 
  let rec sieve lst acc =
    match lst with
    | [] -> (List.rev acc)
    | h::t -> sieve (List.filter t ~f:(fun x -> (x%h)<>0)) (h::acc)
  in sieve (List.range 2 (n+1)) []

let () = test_i "primes test n = (-1)" (primes (-1)) []
let () = test_i "primes test n = 10" (primes 10) [2;3;5;7]
let () = test_i "primes test n = 2" (primes 2) [2]

(* Question 2 
 * Packs consecutive strings that are the same into sub-lists 
 * lst is the list that's having its consecutive elements grouped together
 * lst' is the list passed in to check with the helper function
 * curr is the current consecutive value list
 * acc is the accumulated list of all curr lists
 *)
let group (lst: string list) = 
  let rec group' (lst': string list) curr acc =
    match lst' with
    | [] -> []
    | [x] -> (x::curr)::acc
    | x::(y::_ as t) -> if String.equal x y 
      then group' t (x::curr) acc
      else group' t [] ((x::curr)::acc) 
  in List.rev (group' lst [] [])

let () = test_l "group test 1" (group testlist1) explist1
let () = test_l "group test 2" (group testlist2) explist2
let () = test_l "group test empty list" (group []) []

(* Question 3a 
 * Uses the group function to count the frequency of unique words
 * lst is the list being counted
 * lst' is the list being passed in and processed by the auxillary function
 * acc is the accumulator that builds up a list of (string * int)
 *)
let frequencies_group (lst: string list) = 
  let rec aux (lst': string list list) acc = 
    match lst' with 
    | [] -> acc
    | h::t -> aux t ((List.hd_exn h, List.length h)::acc)
  in List.rev (* Put into alphabetical order*)
    (aux (group (List.sort lst ~compare:String.compare)) []) 

let () = test_t "frequencies_group 1" (frequencies_group testlist1) explist4 
let () = test_t "frequencies_group 2" (frequencies_group testlist2) explist3 
let () = test_t "frequencies_group empty list" (frequencies_group []) [] 


(* Question 3b 
 * Uses the fold_left function to count the frequency of unique words
 * lst is the list being counted
 * tup is the list being checked
 * acc is the accumulator that builds up a list of (string * int)
 *)
let frequencies_fold (lst: string list) =
  let increment_if_match tup n =
    match tup with
    | (k, v) -> 
      if String.equal k n 
      then [(k, v+1)] 
      else [(n,1);(k,v)]
  in 
  let increment acc n =
    match acc with
    | [] -> (n, 1)::acc
    | h::t -> ((increment_if_match h n) @ t);
  in List.rev (* Put into alphabetical order*)
    (List.fold_left (List.sort lst ~compare:String.compare) ~init:[] ~f:increment)

let () = test_t "frequencies_fold 1" (frequencies_fold testlist1) explist4 
let () = test_t "frequencies_fold 2" (frequencies_fold testlist2) explist3 
let () = test_t "frequencies_fold empty list" (frequencies_fold []) [] 

(* Question 4 
 * Reads a line from user input and prints out the frequency of each word
 * Makes each word lowercase so that capitalization does not matter
 *)
let rec process_lines f =
  match In_channel.input_line stdin with
  | None -> ()
  | Some line -> 
    f line;
    process_lines f

(* Prints out tuples
 * list is the list of tuples being printed
 *)
let rec print_tuples lst =
  match lst with
  | [] -> ()
  | (k, v)::t ->
    printf "(%s, %d);" k v;
    print_tuples t

let () = printf "["
let () = process_lines (fun x -> print_tuples
          (frequencies_fold (String.split ~on:' ' (String.lowercase x))))
let () = printf "]\n"


  
