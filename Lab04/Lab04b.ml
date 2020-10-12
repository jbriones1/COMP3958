open Base

let a_list = [("k1", "a");("k2", "b");("k2","c");("k1","d");("k1","e");("k3","f")]

(* Question 2 
 * Splits a number into a list of its digits 
 *
 * n is the number being split *)
let digits n =
  let rec aux n acc =
    match n with
    | 0 -> acc
    | _ -> aux (n / 10) ((n % 10)::acc)
  in aux n []

let question2a = digits 32768
let question2b = digits 1337
let question2c = digits 1409415

(* Question 3 
 * Groups a list of 2-tuples that have the same keys into a key-value list. 
 *
 * compare is the compare function being used
 * lst is the list of 2-tuples being grouped
 * s_lst is the sorted list of 2-tuples based on keys 
 * 
 * kvgroup': auxiliary function that folds the list based on the keys of the
 *            tuples
 * lst' is the lst of tuples being grouped together
 * acc is an accumulator to ensure tail-recursion*)
let kvgroup ~compare (lst: ('a * 'b) list) =
  let s_lst = List.sort lst (fun tup1 tup2 -> 
    match tup1, tup2 with
    | (k1,_),(k2,_) -> compare k1 k2)
  in 
  let rec kvgroup' lst' acc =
    match lst', acc with 
    | [], _-> List.rev acc
    | (k, v)::t, [] -> kvgroup' t ((k, [v])::acc)
    | ((k1, v1)::t1), ((k2, v2)::t2) ->
      if compare k1 k2 = 0
      then kvgroup' t1 ((k2,v1::v2)::t2)
      else kvgroup' t1 (((k1,[v1])::(k2, List.rev v2)::t2))
  in kvgroup' s_lst []

let question3_string = kvgroup ~compare:String.compare 
  [("k1", "a");("k2", "b");("k2","c");("k1","d");("k1","e");("k3","f")]
let question3_int = kvgroup ~compare:Int.compare [(1,"a");(1,"b");(2,"c");(1,"d")]

(* Question 4 *)
(* Power function 
 *
 * a is the base
 * n is the exponent *)
let pow a n =
  let rec pow' n acc =
    if n <= 0
    then acc
    else pow' (n-1) (a*acc)
  in pow' n 1

(* Question 1 
 * Uses filter to remove all multiples of prime numbers.
 *
 * n is the positive integer the program will find primes up to (inclusive)
 * lst is the list of numbers from 2 to n
 * acc is the accumulator to ensure tail recursion *)
let primes n = 
  let rec sieve lst acc =
    match lst with
    | [] -> (List.rev acc)
    | h::t -> 
      if h*h < n
      then sieve (List.filter t ~f:(fun x -> (x%h)<>0)) (h::acc)
      else h::t
  in sieve (List.range 2 (n+1)) []

(* Filters any number below n digits from a list.
 *
 * n is the number of digits it will stop filtering at
 * lst is the list to filter numbers from *)
let rec digit_filter n lst =
  match lst with
  | [] -> []
  | h::t -> 
    if h < (pow 10 (n-1))
    then digit_filter n t
    else h::t

(* Sorts a list of integers from lowest to highest and then combines them into
 * a single number using the list indices for the digits. Index 0 is the most
 * significant digit.
 * 
 * lst is the list of digits to sort from lowest to highest *)
let sorted_digits_to_number lst =
  let s_lst = List.sort lst ~compare: Int.compare in
  let rec to_number lst' acc =
    match lst' with
    | [] -> acc
    | h::t -> to_number t (acc*10 + h)
  in to_number s_lst 0

(* Main question 4 function
 * Finds the largest list of n-digit primes that are permutations of each other
 * n is the digit range to look into. Makes each prime into a tuple, sorts it
 * and then uses the kvgroup function to group all the tuples into tuples of
 * common digits.
 * 
 * find_largest_list: Find the largest length list in the a value of a tuple 
 *                    list
 * lst is the list being searched through
 * curr is the current largest list found
 * 
 * prime_tup: Places the primes into tuples of its digits sorted in lowest
 *            to highest order, back into a single number as the key, with the
 *            prime number as the value 
 * lst' is the list of primes being made into a list of tuples
 * acc is the accumulator of the list of prime tuples*)
let question4 n =
  let rec find_largest_list lst curr =
    match lst with
    | [] -> curr
    | (k, v)::t -> 
      if List.length curr < List.length v
      then find_largest_list t v
      else find_largest_list t curr
  in 
  let rec prime_tup lst' acc =
  match lst' with
  | [] -> List.rev acc
  | h::t -> prime_tup t @@ (sorted_digits_to_number (digits h), h)::acc
  in find_largest_list (kvgroup ~compare:Int.compare 
    (prime_tup (digit_filter (n) (primes @@ pow 10 n)) [])) []

let x = question4 6
let length_of_question4 = List.length x



